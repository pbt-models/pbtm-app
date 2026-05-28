# Equivalence check: the generic fitModel() + spec predict() must reproduce the
# literal nls formulas from the original tab code, fit on each sample dataset
# under default settings (all factor levels, maxFrac = 1, no cleaning, no
# transform). Run with:
#   "C:/Program Files/R/R-4.5.3/bin/Rscript.exe" tests/equivalence.R

suppressPackageStartupMessages(source("global.R"))

tol <- 1e-4
fails <- 0

# compare two named coef lists by relative difference
compareCoefs <- function(label, oldCoefs, newRes) {
  pnames <- names(oldCoefs)
  ok <- TRUE
  for (p in pnames) {
    o <- as.numeric(oldCoefs[[p]])
    n <- as.numeric(newRes[[p]])
    rel <- abs(o - n) / max(abs(o), 1e-12)
    if (!is.finite(rel) || rel > tol) {
      cat(sprintf("  [FAIL] %s$%s: old=%.8g new=%.8g (rel=%.3g)\n", label, p, o, n, rel))
      ok <- FALSE
    }
  }
  # pseudo-R^2 vs Correlation
  oR2 <- attr(oldCoefs, "R2")
  if (!is.null(oR2)) {
    rel <- abs(oR2 - newRes$PseudoR2) / max(abs(oR2), 1e-12)
    if (rel > tol) {
      cat(sprintf("  [FAIL] %s R2: old=%.8g new=%.8g\n", label, oR2, newRes$PseudoR2))
      ok <- FALSE
    }
  }
  if (ok) cat(sprintf("  [OK]   %s (%s)\n", label, paste(pnames, collapse = ", ")))
  ok
}

# fit the literal formula, return coefs with R2 attribute
oldFit <- function(form, df, ranges, response) {
  resolved <- resolveParams(setNames(as.list(rep(NA, length(ranges))), names(ranges)), ranges)
  m <- suppressWarnings(nls(
    form, data = df, start = resolved$start, lower = resolved$lower,
    upper = resolved$upper, algorithm = "port", control = list(warnOnly = TRUE)
  ))
  coefs <- getModelCoefs(m)
  attr(coefs, "R2") <- cor(df[[response]], predict(m))^2
  coefs
}

newFit <- function(spec, df, maxFrac = 1, transform = identity) {
  resolved <- resolveParams(
    setNames(as.list(rep(NA, length(spec$params))), spec$paramNames), spec$params
  )
  pred <- function(d, p) spec$predict(d, p, maxFrac = maxFrac, transform = transform)
  fitModel(pred, df, resolved, spec$response)
}

maxFrac <- 1
cat("== CDF models ==\n")

# Thermal time (germ data)
df <- sampleGermData
old <- oldFit(
  CumFraction ~ maxFrac * pnorm(log10((GermTemp - Tb) * CumTime), log10(ThetaT50), Sigma),
  df, modelSpecs$ThermalTime$params, "CumFraction")
fails <- fails + !compareCoefs("ThermalTime", old, newFit(modelSpecs$ThermalTime, df))

# Hydrotime (germ data)
old <- oldFit(
  CumFraction ~ maxFrac * pnorm(GermWP - (ThetaH / CumTime), PsiB50, Sigma),
  df, modelSpecs$Hydrotime$params, "CumFraction")
fails <- fails + !compareCoefs("Hydrotime", old, newFit(modelSpecs$Hydrotime, df))

# Hydrothermal time (germ data)
old <- oldFit(
  CumFraction ~ maxFrac * pnorm(GermWP - (ThetaHT / ((GermTemp - Tb) * CumTime)), PsiB50, Sigma),
  df, modelSpecs$HydrothermalTime$params, "CumFraction")
fails <- fails + !compareCoefs("HydrothermalTime", old, newFit(modelSpecs$HydrothermalTime, df))

# Aging (aging data)
df <- sampleAgingData
old <- oldFit(
  CumFraction ~ maxFrac * pnorm(AgingTime + ThetaA / CumTime, Pmax50, Sigma, lower.tail = FALSE),
  df, modelSpecs$Aging$params, "CumFraction")
fails <- fails + !compareCoefs("Aging", old, newFit(modelSpecs$Aging, df))

# Promoter (promoter data), transform = none (identity)
df <- samplePromoterData
old <- oldFit(
  CumFraction ~ maxFrac * pnorm(GermPromoterDosage - ThetaP / CumTime, Pb50, Sigma),
  df, modelSpecs$Promoter$params, "CumFraction")
fails <- fails + !compareCoefs("Promoter", old, newFit(modelSpecs$Promoter, df))

# Inhibitor (inhibitor data), transform = none (identity)
df <- sampleInhibitorData
old <- oldFit(
  CumFraction ~ maxFrac * pnorm(GermInhibitorDosage + ThetaI / CumTime, Ib50, Sigma, lower.tail = FALSE),
  df, modelSpecs$Inhibitor$params, "CumFraction")
fails <- fails + !compareCoefs("Inhibitor", old, newFit(modelSpecs$Inhibitor, df))

cat("== Rate models ==\n")

# Build the germination-speed table at 50% the same way the tabs do.
speedTable <- function(df, groups, basis = 50) {
  df |>
    addFracDiff(groups) |>
    interpolateGermSpeed(groups, basis) |>
    dplyr::mutate(GR = round(1 / Time, 6))
}

# Hydropriming (priming data)
df <- speedTable(samplePrimingData, modelSpecs$Hydropriming$groups)
old <- oldFit(
  GR ~ GRi + ((PrimingWP - PsiMin) * PrimingDuration) * Slope,
  df, modelSpecs$Hydropriming$params, "GR")
fails <- fails + !compareCoefs("Hydropriming", old, newFit(modelSpecs$Hydropriming, df))

# Hydrothermal priming (priming data)
df <- speedTable(samplePrimingData, modelSpecs$HydrothermalPriming$groups)
old <- oldFit(
  GR ~ GRi + ((PrimingWP - PsiMin) * (PrimingTemp - Tmin) * PrimingDuration) * Slope,
  df, modelSpecs$HydrothermalPriming$params, "GR")
fails <- fails + !compareCoefs("HydrothermalPriming", old, newFit(modelSpecs$HydrothermalPriming, df))

cat(sprintf("\n%s\n", strrep("-", 40)))
if (fails == 0) {
  cat("ALL MODELS EQUIVALENT ✓\n")
} else {
  cat(sprintf("%d MODEL(S) FAILED EQUIVALENCE\n", fails))
  quit(status = 1)
}
