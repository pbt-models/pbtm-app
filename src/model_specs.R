# ---- Model specifications ---- #
# One spec per model tab. The model itself (parameters, bounds, formula, fitting,
# subpopulation mixtures) comes from the pbtm package, looked up by the spec's
# `pbtm` id; a spec only holds what the app adds on top: UI labels, the docs
# modal, plot styling, and the plotmath parameter annotations. The model
# factory (src/modules/models.R) builds each tab from its spec.
#
# Two families (from pbtm):
#   "cdf"  - cumulative germination CumFraction ~ maxFrac * pnorm(...); one
#            fitted curve per factor level.
#   "rate" - germination rate GR (from a germination-speed table) linear in a
#            priming time theta; fitted as a line.

#' @description spec constructor: UI config plus model metadata from pbtm
#' @param pbtm the pbtm model id (see pbtm::pbtm_models())
#' @param factorLabels named chr vector: checkbox-group label per factor column
#' @param annotate function(res, transform) -> list of plotmath strings for the
#'   fit overlay; `res` is fitValues(fit)
#' @param plot model-specific plotting config (see usage in plot_helpers.R)
modelSpec <- function(label, pbtm, factorLabels, annotate, plot, doc = NULL) {
  model <- pbtm::pbtm_models(pbtm)
  stopifnot(
    is.function(annotate),
    setequal(names(factorLabels), model$factors)
  )
  list(
    label = label,
    pbtm = pbtm,
    family = model$family,
    factors = model$factors,
    factorLabels = factorLabels[model$factors],
    paramNames = model$param_names,
    # rate models group the speed table by TrtID + the treatment factors
    groups = model$groups,
    transformCol = model$transform_col,
    # every cumulative model can be fit as a mixture of subpopulations
    subpop = model$family == "cdf",
    # log-normal in time (thermal time): exactly linear on log time x probit
    logNormal = isTRUE(model$normalized$log),
    # rate models: the priming time on the plot's x axis, theta(data, params)
    theta = model$theta,
    annotate = annotate,
    plot = plot,
    doc = doc
  )
}

# Model defs -------------------------------------------------------------------

modelSpecs <- list(
  ## 1. Germination ----
  # Germination model has its own module and is not driven by these specs

  ## 2. Thermal time ----
  ThermalTime = modelSpec(
    label = "Thermal time",
    pbtm = "thermal_time",
    factorLabels = c(GermTemp = "Included temperature levels:"),
    doc = model_docs$thermal_time,
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
      fitTitle = "Cumulative germination and thermal time sub-optimal model fit",
      normLab = "Thermal time, (T - Tb) × t"
    )
  ),

  ## 3. Hydrotime ----
  Hydrotime = modelSpec(
    label = "Hydrotime",
    pbtm = "hydrotime",
    factorLabels = c(GermWP = "Included water potential levels:"),
    doc = model_docs$hydrotime,
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
      fitTitle = "Cumulative germination and hydrotime model fit",
      normLab = "Base water potential, ψ - θH / t"
    )
  ),

  ## 4. Hydrothermal time ----
  HydrothermalTime = modelSpec(
    label = "Hydrothermal time",
    pbtm = "hydrothermal_time",
    factorLabels = c(
      GermWP = "Included water potential levels:",
      GermTemp = "Included temperature levels:"
    ),
    doc = model_docs$hydrothermal_time,
    annotate = function(res, transform = identity) {
      list(
        paste0("~~theta[HT]==", signif(res$theta_ht, 4)),
        paste0("~~T[b]==", signif(res$t_b, 4)),
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
      fitTitle = "Cumulative germination and hydrothermal time model fit",
      normLab = "Base water potential, ψ - θHT / ((T - Tb) × t)"
    )
  ),

  ## 5. Hydropriming ----
  Hydropriming = modelSpec(
    label = "Hydropriming",
    pbtm = "hydropriming",
    factorLabels = c(
      PrimingWP = "Included priming water potential levels:",
      PrimingDuration = "Included priming duration levels:"
    ),
    doc = model_docs$hydropriming,
    annotate = function(res, transform = identity) {
      list(
        paste0("~~psi[min](50)==", signif(res$psi_min, 4)),
        paste0("~~GR[i]==", signif(res$gr_i, 4)),
        paste0("~~R^2==", signif(res$PseudoR2, 3))
      )
    },
    plot = list(
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

  ## 6. Hydrothermal priming ----
  HydrothermalPriming = modelSpec(
    label = "Hydrothermal priming",
    pbtm = "hydrothermal_priming",
    factorLabels = c(
      PrimingTemp = "Included priming temperature levels:",
      PrimingWP = "Included priming water potential levels:",
      PrimingDuration = "Included priming duration levels:"
    ),
    doc = model_docs$hydrothermal_priming,
    annotate = function(res, transform = identity) {
      list(
        paste0("~~t[min]==", signif(res$t_min, 4)),
        paste0("~~psi[min](50)==", signif(res$psi_min, 4)),
        paste0("~~GR[i]==", signif(res$gr_i, 4)),
        paste0("~~R^2==", signif(res$PseudoR2, 3))
      )
    },
    plot = list(
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
  ),

  ## 7. Aging ----
  Aging = modelSpec(
    label = "Aging",
    pbtm = "aging",
    factorLabels = c(AgingTime = "Included aging times:"),
    doc = model_docs$aging,
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
      fitTitle = "Cumulative germination and aging model fit",
      normLab = "Aging threshold, aging time + θAge / t"
    )
  ),

  ## 8. Promoter ----
  Promoter = modelSpec(
    label = "Promoter",
    pbtm = "promoter",
    factorLabels = c(GermPromoterDosage = "Included promoter dosages:"),
    doc = model_docs$promoters,
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
      fitTitle = "Cumulative germination and promoter model fit",
      normLab = "Promoter threshold, dose - θP / t"
    )
  ),

  ## 9. Inhibitor ----
  Inhibitor = modelSpec(
    label = "Inhibitor",
    pbtm = "inhibitor",
    factorLabels = c(GermInhibitorDosage = "Included inhibitor dosages:"),
    doc = model_docs$inhibitors,
    annotate = function(res, transform = identity) {
      i_b50 <- if (identical(transform, log10)) 10^res$i_b50 else res$i_b50
      list(
        paste0("~~theta[I]==", signif(res$theta_i, 4)),
        paste0("~~I[b][50]==", signif(i_b50, 4)),
        paste0("~~sigma==", signif(res$sigma, 4)),
        paste0("~~R^2==", signif(res$PseudoR2, 3))
      )
    },
    plot = list(
      colorVar = "GermInhibitorDosage",
      colorLab = "Inhibitor dosage",
      legendReverse = FALSE,
      fitTitle = "Cumulative germination and inhibitor model fit",
      normLab = "Inhibitor threshold, dose + θI / t"
    )
  )
)

# The list names match the per-model columns in data/column-validation.csv,
# which drive each model's required-column check. Tag each spec with its name.
for (.nm in names(modelSpecs)) {
  modelSpecs[[.nm]]$id <- .nm
}
rm(.nm)
