# ---- Model fitting (via the pbtm package) ---- #
# All model math, fitting, and subpopulation mixtures live in the pbtm package
# (github.com/pbt-models/pbtm). This file adapts pbtm to the app's conventions:
# a fit is a `pbtm_fit` object (a list) on success or a character error message
# on failure, so reactives can keep showing the last good fit.

#' @description fit a spec's model with pbtm, capturing errors and warnings
#' @param spec a model spec (see model_specs.R); `spec$pbtm` is the pbtm model id
#' @param data working data: time courses for cdf models, or a germination-rate
#'   table with a `GR` column for rate models
#' @param maxFrac maximum cumulative fraction (0-1]
#' @param logDose apply the log10 dosage transform (promoter/inhibitor only)
#' @param subpops number of subpopulations (integer) or "auto"
#' @param fixed named list of user-pinned parameter values (NA = estimate)
#' @returns a `pbtm_fit`, with any pbtm warnings in `$warnings`, or an error
#'   string
fitPbtm <- function(
  spec,
  data,
  maxFrac = 1,
  logDose = FALSE,
  subpops = 1,
  fixed = NULL
) {
  fixed <- Filter(truthy, as.list(fixed))
  fixed <- lapply(fixed, as.numeric)
  warnings <- character()
  fit <- tryCatch(
    withCallingHandlers(
      pbtm::fit_pbtm(
        data,
        spec$pbtm,
        max_frac = maxFrac,
        fixed = if (length(fixed) > 0) fixed,
        subpops = subpops,
        dose_transform = if (logDose) "log10" else "none"
      ),
      warning = function(w) {
        warnings <<- c(warnings, cli::ansi_strip(conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) cli::ansi_strip(conditionMessage(e))
  )
  if (is.list(fit)) {
    fit$warnings <- unique(warnings)
  }
  fit
}

#' @description the fitted parameters plus the pseudo-R^2 as a flat named list,
#'   the shape the specs' `annotate()` and plot `theta()` functions expect
#' @param fit a `pbtm_fit`
fitValues <- function(fit) {
  c(as.list(coef(fit)), PseudoR2 = fit$stats$pseudo_r2)
}

#' @description the dosage transform a fit used, as a function
#' @param fit a `pbtm_fit`
fitTransform <- function(fit) {
  if (identical(fit$dose_transform, "log10")) log10 else identity
}
