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
    dplyr::mutate(GR = 1 / Time)
}

dataFor <- list(
  ThermalTime = sample_data$thermal_time$data,
  Hydrotime = sample_data$hydrotime$data,
  HydrothermalTime = sample_data$hydrothermal_time$data,
  Aging = sample_data$aging$data,
  Promoter = sample_data$promoter$data,
  Inhibitor = sample_data$inhibitor$data
)
primingDataFor <- list(
  Hydropriming = sample_data$hydropriming$data,
  HydrothermalPriming = sample_data$hydrothermal_priming$data
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
for (nm in names(primingDataFor)) {
  spec <- modelSpecs[[nm]]
  df <- speedTable(primingDataFor[[nm]], spec$groups)
  res <- fitSpec(spec, df)
  check(paste(nm, "fits"), is.list(res))
  p <- buildRatePlot(spec, df, res)
  check(paste(nm, "plot builds"), buildsClean(p))
  # the fitted line must be drawn (it silently vanished when the plot looked up
  # parameters under stale names)
  abline <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomAbline"), TRUE))
  lineData <- if (length(abline) == 1) ggplot2::layer_data(p, abline)
  check(
    paste(nm, "plot draws the fitted line"),
    !is.null(lineData) && nrow(lineData) == 1 &&
      isTRUE(all.equal(lineData$slope, res$slope)) &&
      isTRUE(all.equal(lineData$intercept, res$gr_i))
  )
}

cat(sprintf("\n%s\n", strrep("-", 40)))
if (ok) {
  cat("RUNTIME PLOT CHECKS PASSED ✓\n")
} else {
  cat("RUNTIME PLOT CHECKS FAILED\n")
  quit(status = 1)
}
