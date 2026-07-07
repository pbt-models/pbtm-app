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
    doc = doc
  )
}


modelSpecs <- list(
  # Germination model has its own module and is not driven by these specs

  # ---- CDF / threshold-distribution models ---- #

  ThermalTime = modelSpec(
    id = "thermalTime",
    label = "Thermal time",
    family = "cdf",
    factors = "GermTemp",
    factorLabels = c(GermTemp = "Included temperature levels:"),
    subpop = TRUE,
    subpopParam = "theta_t50",
    doc = model_docs$thermal_time,
    params = list(
      t_b = c(0, 6, 20),
      theta_t50 = c(3, 1000, 5e19),
      sigma = c(0.0005, 1, 35)
    ),
    predict = function(data, p, maxFrac = 1, transform = identity) {
      maxFrac *
        pnorm(
          q = log10((data$GermTemp - p$t_b) * data$CumTime),
          mean = log10(p$theta_t50),
          sd = p$sigma
        )
    },
    annotate = function(res, transform = identity) {
      list(
        paste0("~~T[b]==", signif(res$t_b, 4)),
        paste0("~~theta[T][50]==", signif(res$theta_t50, 4)),
        paste0("~~sigma==", signif(res$sigma, 4)),
        paste0("~~R^2==", signif(res$PseudoR2, 3))
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
    subpopParam = "psi_b50",
    doc = model_docs$hydrotime,
    params = list(
      theta_h = c(1, 60, 1000),
      psi_b50 = c(-5, -0.8, -1e-9),
      sigma = c(1e-4, 0.2, 2)
    ),
    predict = function(data, p, maxFrac = 1, transform = identity) {
      maxFrac *
        pnorm(
          q = data$GermWP - (p$theta_h / data$CumTime),
          mean = p$psi_b50,
          sd = p$sigma
        )
    },
    annotate = function(res, transform = identity) {
      list(
        paste0("~~theta~H==", signif(res$theta_h, 4)),
        paste0("~~psi[b][50]==", signif(res$psi_b50, 4)),
        paste0("~~sigma==", signif(res$sigma, 4)),
        paste0("~~R^2==", signif(res$PseudoR2, 3))
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
    subpopParam = "psi_b50",
    doc = model_docs$hydrothermal_time,
    params = list(
      theta_ht = c(1, 800, 5000),
      tb = c(0, 1, 15),
      psi_b50 = c(-5, -1, 0),
      sigma = c(.0001, .4, 10)
    ),
    predict = function(data, p, maxFrac = 1, transform = identity) {
      maxFrac *
        pnorm(
          q = data$GermWP -
            (p$theta_ht / ((data$GermTemp - p$tb) * data$CumTime)),
          mean = p$psi_b50,
          sd = p$sigma
        )
    },
    annotate = function(res, transform = identity) {
      list(
        paste0("~~theta[HT]==", signif(res$theta_ht, 4)),
        paste0("~~T[b]==", signif(res$tb, 4)),
        paste0("~~psi[b][50]==", signif(res$psi_b50, 4)),
        paste0("~~sigma==", signif(res$sigma, 4)),
        paste0("~~R^2==", signif(res$PseudoR2, 3))
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
    subpopParam = "p_max50",
    doc = model_docs$aging,
    params = list(
      theta_a = c(1, 100, 1000),
      p_max50 = c(1, 10, 1000),
      sigma = c(0.1, 3, 10)
    ),
    predict = function(data, p, maxFrac = 1, transform = identity) {
      maxFrac *
        pnorm(
          q = data$AgingTime + p$theta_a / data$CumTime,
          mean = p$p_max50,
          sd = p$sigma,
          lower.tail = FALSE
        )
    },
    annotate = function(res, transform = identity) {
      list(
        paste0("~~theta~Age==", signif(res$theta_a, 4)),
        paste0("~~p[max][50]==", signif(res$p_max50, 4)),
        paste0("~~sigma==", signif(res$sigma, 4)),
        paste0("~~R^2==", signif(res$PseudoR2, 3))
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
    subpopParam = "p_b50",
    doc = model_docs$promoters,
    params = list(
      theta_p = c(1, 200, 1000),
      p_b50 = c(0.05, 5, 1000),
      sigma = c(.001, 3, 10)
    ),
    predict = function(data, p, maxFrac = 1, transform = identity) {
      maxFrac *
        pnorm(
          q = transform(data$GermPromoterDosage) - p$theta_p / data$CumTime,
          mean = p$p_b50,
          sd = p$sigma
        )
    },
    annotate = function(res, transform = identity) {
      # p_b50 is in log10 units only when the log dosage transform is applied;
      # back-transform for display in that case, otherwise show it as-is
      p_b50 <- if (identical(transform, log10)) 10^res$p_b50 else res$p_b50
      list(
        paste0("~~theta[P]==", signif(res$theta_p, 4)),
        paste0("~~p[b][50]==", signif(p_b50, 4)),
        paste0("~~sigma==", signif(res$sigma, 4)),
        paste0("~~R^2==", signif(res$PseudoR2, 3))
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
    subpopParam = "I_b50",
    doc = model_docs$inhibitors,
    params = list(
      theta_I = c(1, 100, 1000),
      I_b50 = c(0.05, 10, 1000),
      sigma = c(.001, 3, 10)
    ),
    predict = function(data, p, maxFrac = 1, transform = identity) {
      maxFrac *
        pnorm(
          q = transform(data$GermInhibitorDosage) + p$theta_I / data$CumTime,
          mean = p$I_b50,
          sd = p$sigma,
          lower.tail = FALSE
        )
    },
    annotate = function(res, transform = identity) {
      I_b50 <- if (identical(transform, log10)) 10^res$I_b50 else res$I_b50
      list(
        paste0("~~theta[I]==", signif(res$theta_I, 4)),
        paste0("~~I[b][50]==", signif(I_b50, 4)),
        paste0("~~sigma==", signif(res$sigma, 4)),
        paste0("~~R^2==", signif(res$PseudoR2, 3))
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
    doc = model_docs$hydropriming,
    params = list(
      psi_min = c(-10, -1, -0.5),
      GR_i = c(1e-8, 0.001, 0.1),
      slope = c(1e-8, 0.1, 1)
    ),
    predict = function(data, p, maxFrac = 1, transform = identity) {
      theta <- (data$PrimingWP - p$psi_min) * data$PrimingDuration
      p$GR_i + theta * p$slope
    },
    annotate = function(res, transform = identity) {
      list(
        paste0("~~psi[min](50)==", signif(res$psi_min, 4)),
        paste0("~~GR[i]==", signif(res$GR_i, 4)),
        paste0("~~R^2==", signif(res$PseudoR2, 3))
      )
    },
    plot = list(
      theta = function(data, res) {
        (data$PrimingWP - res$psi_min) * data$PrimingDuration
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
    doc = model_docs$hydrothermal_priming,
    params = list(
      t_min = c(0.5, 12, 20),
      psi_min = c(-10, -1, -0.5),
      GR_i = c(1e-8, 0.001, 0.1),
      slope = c(1e-8, 0.1, 1)
    ),
    predict = function(data, p, maxFrac = 1, transform = identity) {
      theta <- (data$PrimingWP - p$psi_min) *
        (data$PrimingTemp - p$t_min) *
        data$PrimingDuration
      p$GR_i + theta * p$slope
    },
    annotate = function(res, transform = identity) {
      list(
        paste0("~~t[min]==", signif(res$t_min, 4)),
        paste0("~~psi[min](50)==", signif(res$psi_min, 4)),
        paste0("~~GR[i]==", signif(res$GR_i, 4)),
        paste0("~~R^2==", signif(res$PseudoR2, 3))
      )
    },
    plot = list(
      theta = function(data, res) {
        (data$PrimingWP - res$psi_min) *
          (data$PrimingTemp - res$t_min) *
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
