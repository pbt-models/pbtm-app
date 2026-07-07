# Check ggplotly conversion works for a CDF and a rate plot (interactive mode).
suppressPackageStartupMessages(source("global.R"))

sampleThermData <- sample_data$thermal_time$data
sampleHydroThermData <- sample_data$hydrothermal_time$data
samplePrimingData <- sample_data$hydropriming$data

fitSpec <- function(spec, df, maxFrac = 1, transform = identity) {
  resolved <- resolveParams(
    setNames(as.list(rep(NA, length(spec$params))), spec$paramNames),
    spec$params
  )
  fitModel(
    function(d, p) spec$predict(d, p, maxFrac = maxFrac, transform = transform),
    df,
    resolved,
    spec$response
  )
}
speedTable <- function(df, groups, basis = 50) {
  df |>
    addFracDiff(groups) |>
    interpolateGermSpeed(groups, basis) |>
    dplyr::mutate(GR = round(1 / Time, 6))
}
ok <- TRUE
chk <- function(lbl, p) {
  built <- !inherits(
    try(plotly::plotly_build(plotly::ggplotly(p)), silent = TRUE),
    "try-error"
  )
  cat(sprintf("  [%s] %s\n", if (built) "OK" else "FAIL", lbl))
  if (!built) ok <<- FALSE
}

spec <- modelSpecs$ThermalTime
chk(
  "ThermalTime ggplotly",
  buildCdfPlot(
    spec,
    sampleThermData,
    fitSpec(spec, sampleThermData),
    1,
    identity,
    interactive = TRUE
  )
)
spec <- modelSpecs$HydrothermalTime
chk(
  "HydrothermalTime ggplotly",
  buildCdfPlot(
    spec,
    sampleHydroThermData,
    fitSpec(spec, sampleHydroThermData),
    1,
    identity,
    interactive = TRUE
  )
)
spec <- modelSpecs$Hydropriming
df <- speedTable(samplePrimingData, spec$groups)
chk(
  "Hydropriming ggplotly",
  buildRatePlot(spec, df, fitSpec(spec, df), interactive = TRUE)
)

cat(if (ok) "GGPLOTLY OK\n" else "GGPLOTLY FAILED\n")
if (!ok) {
  quit(status = 1)
}
