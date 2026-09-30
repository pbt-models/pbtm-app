# Runtime check: fit every model on its sample dataset (via pbtm, through the
# app's fitPbtm adapter) and assemble its plot (buildCdfPlot / buildRatePlot),
# forcing ggplot_build to surface any aes or layer errors. Covers the
# two-factor (hydrothermal time), dosage-transform (promoter, inhibitor) and
# rate (priming) variants. Run from project root:
#   "C:/Program Files/R/R-4.6.1/bin/Rscript.exe" tests/runtime.R

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

cat("== Specs match pbtm ==\n")
for (nm in names(modelSpecs)) {
  spec <- modelSpecs[[nm]]
  check(
    paste(nm, "params match pbtm"),
    identical(spec$paramNames, pbtm::pbtm_models(spec$pbtm)$param_names)
  )
}

cat("== CDF model plots ==\n")
for (nm in names(dataFor)) {
  spec <- modelSpecs[[nm]]
  df <- dataFor[[nm]]
  res <- fitPbtm(spec, df)
  check(paste(nm, "fits"), inherits(res, "pbtm_fit"))
  p <- buildCdfPlot(spec, df, res, 1)
  check(paste(nm, "plot builds (fitted)"), buildsClean(p))
  # also build with no model yet (points only)
  check(
    paste(nm, "plot builds (no fit)"),
    buildsClean(buildCdfPlot(spec, df, NULL, 1))
  )
  # linearizing axes, including a max germination below 100%
  for (sc in list(c("log", "linear"), c("linear", "probit"), c("log", "probit"))) {
    p <- buildCdfPlot(spec, df, res, 0.9, xScale = sc[1], yScale = sc[2])
    check(
      sprintf("%s plot builds (%s time, %s fraction)", nm, sc[1], sc[2]),
      buildsClean(p)
    )
  }
}

cat("== Probit axis linearizes thermal time ==\n")
# on log time x probit, each temperature's fitted curve is a straight line
spec <- modelSpecs$ThermalTime
df <- dataFor$ThermalTime
res <- fitPbtm(spec, df)
curve <- ggplot2::layer_data(
  buildCdfPlot(spec, df, res, 1, xScale = "log", yScale = "probit"),
  2 # points, then the fitted curves (no max-germination band on probit)
)
straight <- vapply(split(curve, curve$group), function(g) {
  summary(lm(y ~ x, data = g))$r.squared > 0.9999
}, logical(1))
check("thermal time curves are straight on log/probit axes", all(straight))

cat("== Normalized plots ==\n")
# every treatment collapses onto one population line, straight on probit
# (log thermal time for thermal time; the threshold axis itself otherwise)
for (nm in names(dataFor)) {
  spec <- modelSpecs[[nm]]
  df <- dataFor[[nm]]
  res <- fitPbtm(spec, df, logDose = nm %in% c("Promoter", "Inhibitor"))
  for (sc in list(c("linear", "linear"), c("linear", "probit"), c("log", "probit"))) {
    p <- buildNormalizedPlot(spec, df, res, 0.9, xScale = sc[1], yScale = sc[2])
    check(
      sprintf("%s normalized plot builds (%s x, %s fraction)", nm, sc[1], sc[2]),
      buildsClean(p)
    )
  }
  line <- ggplot2::layer_data(
    buildNormalizedPlot(spec, df, res, 1, xScale = "log", yScale = "probit"),
    2 # median line, then the population curve
  )
  check(
    paste(nm, "normalized curve is straight on probit"),
    summary(lm(y ~ x, data = line))$r.squared > 0.9999
  )
}
mix <- fitPbtm(
  modelSpecs$ThermalTime,
  sample_data$thermal_time_subpop$data,
  subpops = 2
)
msg <- tryCatch(
  buildNormalizedPlot(
    modelSpecs$ThermalTime,
    sample_data$thermal_time_subpop$data,
    mix,
    1
  ),
  error = conditionMessage
)
check(
  "normalized plot explains it needs a single population",
  is.character(msg) && grepl("single-population", msg)
)

# promoter/inhibitor with log transform
for (nm in c("Promoter", "Inhibitor")) {
  spec <- modelSpecs[[nm]]
  df <- dataFor[[nm]]
  res <- fitPbtm(spec, df, logDose = TRUE)
  check(paste(nm, "fits (log transform)"), identical(res$dose_transform, "log10"))
  p <- buildCdfPlot(spec, df, res, 1)
  check(paste(nm, "plot builds (log transform)"), buildsClean(p))
}

cat("== Fit problems are reported, not fatal ==\n")
# promoter on the linear dose scale: theta_p runs into its upper bound
res <- fitPbtm(modelSpecs$Promoter, dataFor$Promoter)
check("bound warning captured", any(grepl("theta_p", res$warnings)))
res <- fitPbtm(modelSpecs$Hydrotime, dataFor$ThermalTime)
check("missing column gives an error string", is.character(res))

cat("== Rate model plots ==\n")
for (nm in names(primingDataFor)) {
  spec <- modelSpecs[[nm]]
  df <- pbtm::germ_speed(primingDataFor[[nm]], 0.5, groups = spec$groups)
  res <- fitPbtm(spec, df)
  check(paste(nm, "fits"), inherits(res, "pbtm_fit"))
  p <- buildRatePlot(spec, df, res)
  check(paste(nm, "plot builds"), buildsClean(p))
  # the fitted line must be drawn (it silently vanished when the plot looked up
  # parameters under stale names)
  abline <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomAbline"), TRUE))
  lineData <- if (length(abline) == 1) ggplot2::layer_data(p, abline)
  check(
    paste(nm, "plot draws the fitted line"),
    !is.null(lineData) && nrow(lineData) == 1 &&
      isTRUE(all.equal(lineData$slope, coef(res)[["slope"]])) &&
      isTRUE(all.equal(lineData$intercept, coef(res)[["gr_i"]]))
  )
}

cat(sprintf("\n%s\n", strrep("-", 40)))
if (ok) {
  cat("RUNTIME PLOT CHECKS PASSED ✓\n")
} else {
  cat("RUNTIME PLOT CHECKS FAILED\n")
  quit(status = 1)
}
