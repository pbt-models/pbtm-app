# Runtime check: fit every model (via the validated fitModel) and assemble its
# plot (buildCdfPlot / buildRatePlot), forcing ggplot_build to surface any aes
# or layer errors. This exercises the new data-first plotting for all 8 models,
# including the two-factor (hydrothermal time), dosage-transform (promoter,
# inhibitor) and rate (priming) variants. Run from project root:
#   "C:/Program Files/R/R-4.5.3/bin/Rscript.exe" tests/runtime.R

suppressPackageStartupMessages(source("global.R"))

ok <- TRUE
check <- function(label, cond) {
  cat(sprintf("  [%s] %s\n", if (isTRUE(cond)) "OK" else "FAIL", label))
  if (!isTRUE(cond)) ok <<- FALSE
}
buildsClean <- function(p) {
  inherits(p, "ggplot") &&
    !inherits(try(ggplot2::ggplot_build(p), silent = TRUE), "try-error")
}

# fit a spec on a data frame under default settings; returns results list
fitSpec <- function(spec, df, maxFrac = 1, transform = identity) {
  resolved <- resolveParams(
    setNames(as.list(rep(NA, length(spec$params))), spec$paramNames),
    spec$params
  )
  pred <- function(d, p) {
    spec$predict(d, p, maxFrac = maxFrac, transform = transform)
  }
  fitModel(pred, df, resolved, spec$response)
}

speedTable <- function(df, groups, basis = 50) {
  df |>
    addFracDiff(groups) |>
    interpolateGermSpeed(groups, basis) |>
    dplyr::mutate(GR = round(1 / Time, 6))
}

dataFor <- list(
  ThermalTime = sampleGermData,
  Hydrotime = sampleGermData,
  HydrothermalTime = sampleGermData,
  Aging = sampleAgingData,
  Promoter = samplePromoterData,
  Inhibitor = sampleInhibitorData
)

cat("== CDF model plots ==\n")
for (nm in names(dataFor)) {
  spec <- modelSpecs[[nm]]
  df <- dataFor[[nm]]
  res <- fitSpec(spec, df)
  check(paste(nm, "fits"), is.list(res))
  p <- buildCdfPlot(spec, df, res, 1, identity)
  check(paste(nm, "plot builds (fitted)"), buildsClean(p))
  # also build with no model yet (points only)
  check(
    paste(nm, "plot builds (no fit)"),
    buildsClean(buildCdfPlot(spec, df, NULL, 1, identity))
  )
}

# promoter/inhibitor with log transform
for (nm in c("Promoter", "Inhibitor")) {
  spec <- modelSpecs[[nm]]
  df <- dataFor[[nm]]
  res <- fitSpec(spec, df, transform = log10)
  p <- buildCdfPlot(spec, df, res, 1, log10)
  check(paste(nm, "plot builds (log transform)"), buildsClean(p))
}

cat("== Rate model plots ==\n")
for (nm in c("Hydropriming", "HydrothermalPriming")) {
  spec <- modelSpecs[[nm]]
  df <- speedTable(samplePrimingData, spec$groups)
  res <- fitSpec(spec, df)
  check(paste(nm, "fits"), is.list(res))
  p <- buildRatePlot(spec, df, res)
  check(paste(nm, "plot builds"), buildsClean(p))
}

cat(sprintf("\n%s\n", strrep("-", 40)))
if (ok) {
  cat("RUNTIME PLOT CHECKS PASSED ✓\n")
} else {
  cat("RUNTIME PLOT CHECKS FAILED\n")
  quit(status = 1)
}
