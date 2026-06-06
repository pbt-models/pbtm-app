# ---- Model specifications ---- #
# One spec per nls model. Everything that differs between the 8 model tabs lives
# here; the shared behaviour lives in the model factory (model-module.R), the
# fitting core (fit.R), and the plot helpers (plot-helpers.R). The `predict`
# function is the single source of truth for each model: it is reused by the nls
# fit, the fitted-curve overlay, the pseudo-R^2, and (for CDF models) the
# subpopulation mixture.
#
# Two families:
#   "cdf"  - fit CumFraction ~ maxFrac * pnorm(z); one curve per factor level.
#   "rate" - first reduce to a germination-rate table, then fit GR ~ linear(theta).
#
# predict(data, p, maxFrac = 1, transform = identity) -> numeric vector
#   data:      data frame with the predictor columns
#   p:         named list of parameter values
#   maxFrac:   scalar max cumulative fraction (ignored by rate models)
#   transform: dosage transform for promoter/inhibitor ("none" = identity,
#              "log" = log10); ignored by other models

#' @description spec constructor with light validation / defaults
#' @param factorLabels named chr vector: checkbox-group label per factor column
#' @param plot model-specific plotting config (see usage in model-module.R)
#' @param annotate function(res) -> list of plotmath strings for the fit overlay
modelSpec <- function(
  id,
  label,
  family,
  factors,
  factorLabels,
  params,
  predict,
  annotate,
  plot,
  response = if (family == "rate") "GR" else "CumFraction",
  groups = NULL,
  transformCol = NULL,
  subpop = FALSE,
  subpopParam = NULL,
  doc = NULL,
  tabInfo = NULL
) {
  stopifnot(
    family %in% c("cdf", "rate"),
    is.function(predict),
    is.function(annotate)
  )
  list(
    id = id,
    label = label,
    family = family,
    factors = factors,
    factorLabels = factorLabels,
    params = params,
    paramNames = names(params),
    predict = predict,
    annotate = annotate,
    plot = plot,
    response = response,
    # rate models group the speed table by TrtID + the treatment factors
    groups = groups %||% c("TrtID", factors),
    transformCol = transformCol,
    subpop = subpop,
    # parameter that distinguishes subpopulations (the threshold / b50 param);
    # the mixture fitter spreads starting values across components along it
    subpopParam = subpopParam,
    doc = doc,
    tabInfo = tabInfo
  )
}


modelSpecs <- list(
  # ---- CDF / threshold-distribution models ---- #

  ThermalTime = modelSpec(
    id = "thermalTime",
    label = "Thermal time",
    family = "cdf",
    factors = "GermTemp",
    factorLabels = c(GermTemp = "Included temperature levels:"),
    subpop = TRUE,
    subpopParam = "ThetaT50",
    doc = "md/thermal_time.md",
    tabInfo = "The thermal time model assumes a dataset with germination temperature as a treatment condition. If you have additional treatments, remove them using the treatment filters below or the model may give unreliable results. Note: the model may fail to converge under some conditions; try adjusting the data or model constraints.",
    params = list(
      Tb = c(0, 6, 20),
      ThetaT50 = c(3, 1000, 5e19),
      Sigma = c(0.0005, 1, 35)
    ),
    predict = function(data, p, maxFrac = 1, transform = identity) {
      maxFrac *
        pnorm(
          q = log10((data$GermTemp - p$Tb) * data$CumTime),
          mean = log10(p$ThetaT50),
          sd = p$Sigma
        )
    },
    annotate = function(res, transform = identity) {
      list(
        sprintf("~~T[b]==%.2f", res$Tb),
        sprintf("~~theta[T][50]==%.1f", res$ThetaT50),
        sprintf("~~sigma==%.3f", res$Sigma),
        sprintf("~~R^2==%.2f", res$PseudoR2)
      )
    },
    plot = list(
      colorVar = "GermTemp",
      colorLab = "Temperature",
      legendReverse = TRUE,
      fitTitle = "Cumulative germination and thermal time sub-optimal model fit"
    )
  ),

  Hydrotime = modelSpec(
    id = "hydrotime",
    label = "Hydrotime",
    family = "cdf",
    factors = "GermWP",
    factorLabels = c(GermWP = "Included water potential levels:"),
    subpop = TRUE,
    subpopParam = "PsiB50",
    doc = "md/hydrotime.md",
    tabInfo = "The hydrotime model assumes a dataset with germination water potential as a treatment condition. If you have additional treatments, the model will average across them and you may get unreliable results. Note: the model may fail to converge under some conditions; try adjusting the data or model constraints.",
    params = list(
      ThetaH = c(1, 60, 1000),
      PsiB50 = c(-5, -0.8, -1e-9),
      Sigma = c(1e-4, 0.2, 2)
    ),
    predict = function(data, p, maxFrac = 1, transform = identity) {
      maxFrac *
        pnorm(
          q = data$GermWP - (p$ThetaH / data$CumTime),
          mean = p$PsiB50,
          sd = p$Sigma
        )
    },
    annotate = function(res, transform = identity) {
      list(
        sprintf("~~theta~H==%.2f", res$ThetaH),
        sprintf("~~Psi[b][50]==%.2f", res$PsiB50),
        sprintf("~~sigma==%.3f", res$Sigma),
        sprintf("~~R^2==%.2f", res$PseudoR2)
      )
    },
    plot = list(
      colorVar = "GermWP",
      colorLab = "Water potential",
      legendReverse = TRUE,
      fitTitle = "Cumulative germination and hydrotime model fit"
    )
  ),

  HydrothermalTime = modelSpec(
    id = "hydrothermalTime",
    label = "Hydrothermal time",
    family = "cdf",
    factors = c("GermWP", "GermTemp"),
    factorLabels = c(
      GermWP = "Included water potential levels:",
      GermTemp = "Included temperature levels:"
    ),
    subpop = TRUE,
    subpopParam = "PsiB50",
    doc = "md/hydrothermal-time.md",
    tabInfo = "The hydrothermal time model assumes a dataset with germination temperature and water potential as treatment conditions. If you have additional treatments, the model will average across them and you may get unreliable results. Note: the model may fail to converge under some conditions; try adjusting the data or model constraints.",
    params = list(
      ThetaHT = c(1, 800, 5000),
      Tb = c(0, 1, 15),
      PsiB50 = c(-5, -1, 0),
      Sigma = c(.0001, .4, 10)
    ),
    predict = function(data, p, maxFrac = 1, transform = identity) {
      maxFrac *
        pnorm(
          q = data$GermWP -
            (p$ThetaHT / ((data$GermTemp - p$Tb) * data$CumTime)),
          mean = p$PsiB50,
          sd = p$Sigma
        )
    },
    annotate = function(res, transform = identity) {
      list(
        sprintf("~~theta~HT==%.2f", res$ThetaHT),
        sprintf("~~T[b]==%.2f", res$Tb),
        sprintf("~~psi[b][50]==%.3f", res$PsiB50),
        sprintf("~~sigma==%.3f", res$Sigma),
        sprintf("~~R^2==%.2f", res$PseudoR2)
      )
    },
    plot = list(
      colorVar = "GermWP",
      colorLab = "Water potential",
      shapeVar = "GermTemp",
      shapeLab = "Temperature",
      lineTypeVar = "GermTemp",
      legendReverse = TRUE,
      fitTitle = "Cumulative germination and hydrothermal time model fit"
    )
  ),

  Aging = modelSpec(
    id = "aging",
    label = "Aging",
    family = "cdf",
    factors = "AgingTime",
    factorLabels = c(AgingTime = "Included aging times:"),
    subpop = TRUE,
    subpopParam = "Pmax50",
    doc = "md/aging-time.md",
    tabInfo = "The aging model assumes a dataset with aging (natural, controlled deterioration, or accelerated aging) as a treatment condition. Filter out additional treatments or you may get unreliable results. Note: the model may fail to converge under some conditions; try adjusting the data or model constraints.",
    params = list(
      ThetaA = c(1, 100, 1000),
      Pmax50 = c(1, 10, 1000),
      Sigma = c(0.1, 3, 10)
    ),
    predict = function(data, p, maxFrac = 1, transform = identity) {
      maxFrac *
        pnorm(
          q = data$AgingTime + p$ThetaA / data$CumTime,
          mean = p$Pmax50,
          sd = p$Sigma,
          lower.tail = FALSE
        )
    },
    annotate = function(res, transform = identity) {
      list(
        sprintf("~~theta~Age==%.2f", res$ThetaA),
        sprintf("~~Pmax[50]==%.2f", res$Pmax50),
        sprintf("~~sigma==%.3f", res$Sigma),
        sprintf("~~R^2==%.2f", res$PseudoR2)
      )
    },
    plot = list(
      colorVar = "AgingTime",
      colorLab = "Aging time",
      legendReverse = FALSE,
      fitTitle = "Cumulative germination and aging model fit"
    )
  ),

  Promoter = modelSpec(
    id = "promoter",
    label = "Promoter",
    family = "cdf",
    factors = "GermPromoterDosage",
    factorLabels = c(GermPromoterDosage = "Included promoter dosages:"),
    transformCol = "GermPromoterDosage",
    subpop = TRUE,
    subpopParam = "Pb50",
    doc = "md/promoters.md",
    tabInfo = "The promoter model assumes a dataset with a promoter at different dosages (higher values speed up germination) as a treatment condition. Filter out additional treatments or you may get unreliable results. Note: the model may fail to converge under some conditions; try adjusting the data or model constraints.",
    params = list(
      ThetaP = c(1, 200, 1000),
      Pb50 = c(0.05, 5, 1000),
      Sigma = c(.001, 3, 10)
    ),
    predict = function(data, p, maxFrac = 1, transform = identity) {
      maxFrac *
        pnorm(
          q = transform(data$GermPromoterDosage) - p$ThetaP / data$CumTime,
          mean = p$Pb50,
          sd = p$Sigma
        )
    },
    annotate = function(res, transform = identity) {
      # Pb50 is in log10 units only when the log dosage transform is applied;
      # back-transform for display in that case, otherwise show it as-is
      pb50 <- if (identical(transform, log10)) 10^res$Pb50 else res$Pb50
      list(
        sprintf("~~theta[P]==%.2f", res$ThetaP),
        sprintf("~~P[b](50)==%.2f", pb50),
        sprintf("~~sigma==%.3f", res$Sigma),
        sprintf("~~R^2==%.2f", res$PseudoR2)
      )
    },
    plot = list(
      colorVar = "GermPromoterDosage",
      colorLab = "Promoter dosage",
      legendReverse = FALSE,
      fitTitle = "Cumulative germination and promoter model fit"
    )
  ),

  Inhibitor = modelSpec(
    id = "inhibitor",
    label = "Inhibitor",
    family = "cdf",
    factors = "GermInhibitorDosage",
    factorLabels = c(GermInhibitorDosage = "Included inhibitor dosages:"),
    transformCol = "GermInhibitorDosage",
    subpop = TRUE,
    subpopParam = "Ib50",
    doc = "md/inhibitors.md",
    tabInfo = "The inhibitor model assumes a dataset with an inhibitor at different dosages (higher values delay germination) as a treatment condition. Filter out additional treatments or you may get unreliable results. Note: the model may fail to converge under some conditions; try adjusting the data or model constraints.",
    params = list(
      ThetaI = c(1, 100, 1000),
      Ib50 = c(0.05, 10, 1000),
      Sigma = c(.001, 3, 10)
    ),
    predict = function(data, p, maxFrac = 1, transform = identity) {
      maxFrac *
        pnorm(
          q = transform(data$GermInhibitorDosage) + p$ThetaI / data$CumTime,
          mean = p$Ib50,
          sd = p$Sigma,
          lower.tail = FALSE
        )
    },
    annotate = function(res, transform = identity) {
      ib50 <- if (identical(transform, log10)) 10^res$Ib50 else res$Ib50
      list(
        sprintf("~~theta[I]==%.2f", res$ThetaI),
        sprintf("~~I[b](50)==%.2f", ib50),
        sprintf("~~sigma==%.3f", res$Sigma),
        sprintf("~~R^2==%.2f", res$PseudoR2)
      )
    },
    plot = list(
      colorVar = "GermInhibitorDosage",
      colorLab = "Inhibitor dosage",
      legendReverse = FALSE,
      fitTitle = "Cumulative germination and inhibitor model fit"
    )
  ),

  # ---- Rate-based priming models ---- #

  Hydropriming = modelSpec(
    id = "hydropriming",
    label = "Hydropriming",
    family = "rate",
    factors = c("PrimingWP", "PrimingDuration"),
    factorLabels = c(
      PrimingWP = "Included priming water potential levels:",
      PrimingDuration = "Included priming duration levels:"
    ),
    groups = c("TrtID", "PrimingWP", "PrimingDuration"),
    subpop = FALSE,
    doc = "md/hydropriming-time.md",
    tabInfo = "The hydropriming model assumes a dataset with priming water potential and priming duration as treatment conditions. If you have additional treatments, the model will average across them and you may get unreliable results. Note: the model may fail to converge under some conditions; try adjusting the data or model constraints.",
    params = list(
      PsiMin = c(-10, -1, -0.5),
      GRi = c(1e-8, 0.001, 0.1),
      Slope = c(1e-8, 0.1, 1)
    ),
    predict = function(data, p, maxFrac = 1, transform = identity) {
      theta <- (data$PrimingWP - p$PsiMin) * data$PrimingDuration
      p$GRi + theta * p$Slope
    },
    annotate = function(res, transform = identity) {
      list(
        sprintf("psi[min](50)==%.2f", res$PsiMin),
        sprintf("GRi==%.4f", res$GRi),
        sprintf("~~R^2==%.2f", res$PseudoR2)
      )
    },
    plot = list(
      theta = function(data, res) {
        (data$PrimingWP - res$PsiMin) * data$PrimingDuration
      },
      xlab = "Hydropriming time",
      colorVar = "PrimingWP",
      colorLab = "Water potential",
      shapeVar = "PrimingDuration",
      shapeLab = "Duration",
      fixedSize = 4,
      legendReverse = TRUE,
      fitTitle = "Germination rates and hydropriming model fit"
    )
  ),

  HydrothermalPriming = modelSpec(
    id = "hydrothermalPriming",
    label = "Hydrothermal priming",
    family = "rate",
    factors = c("PrimingTemp", "PrimingWP", "PrimingDuration"),
    factorLabels = c(
      PrimingTemp = "Included priming temperature levels:",
      PrimingWP = "Included priming water potential levels:",
      PrimingDuration = "Included priming duration levels:"
    ),
    groups = c("TrtID", "PrimingTemp", "PrimingWP", "PrimingDuration"),
    subpop = FALSE,
    doc = "md/hydrothermal-priming-time.md",
    tabInfo = "The hydrothermal priming model assumes a dataset with priming temperature, priming water potential, and priming duration as treatment conditions. If you have additional treatments, the model will average across them and you may get unreliable results. Note: the model may fail to converge under some conditions; try adjusting the data or model constraints.",
    params = list(
      Tmin = c(0.5, 12, 20),
      PsiMin = c(-10, -1, -0.5),
      GRi = c(1e-8, 0.001, 0.1),
      Slope = c(1e-8, 0.1, 1)
    ),
    predict = function(data, p, maxFrac = 1, transform = identity) {
      theta <- (data$PrimingWP - p$PsiMin) *
        (data$PrimingTemp - p$Tmin) *
        data$PrimingDuration
      p$GRi + theta * p$Slope
    },
    annotate = function(res, transform = identity) {
      list(
        sprintf("T[min]==%.2f", res$Tmin),
        sprintf("psi[min](50)==%.2f", res$PsiMin),
        sprintf("GRi==%.4f", res$GRi),
        sprintf("~~R^2==%.2f", res$PseudoR2)
      )
    },
    plot = list(
      theta = function(data, res) {
        (data$PrimingWP - res$PsiMin) *
          (data$PrimingTemp - res$Tmin) *
          data$PrimingDuration
      },
      xlab = "Hydrothermal priming time",
      colorVar = "PrimingWP",
      colorLab = "Water potential",
      shapeVar = "PrimingTemp",
      shapeLab = "Temperature",
      sizeVar = "PrimingDuration",
      sizeLab = "Duration",
      legendReverse = TRUE,
      fitTitle = "Germination rates and hydrothermal priming model fit"
    )
  )
)

# The list names match the per-model columns in data/column-validation.csv,
# which drive each model's required-column check. Tag each spec with its name.
for (.nm in names(modelSpecs)) {
  modelSpecs[[.nm]]$modelCol <- .nm
}
rm(.nm)
