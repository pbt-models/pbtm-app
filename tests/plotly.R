# Check ggplotly conversion works for CDF (single and mixture) and rate plots
# (interactive mode). Run from project root:
#   "C:/Program Files/R/R-4.6.1/bin/Rscript.exe" tests/plotly.R
suppressPackageStartupMessages(source("global.R"))

sampleThermData <- sample_data$thermal_time$data
sampleHydroThermData <- sample_data$hydrothermal_time$data
samplePrimingData <- sample_data$hydropriming$data
sampleSubpopData <- sample_data$thermal_time_subpop$data

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
    fitPbtm(spec, sampleThermData),
    1,
    interactive = TRUE
  )
)
chk(
  "ThermalTime 2-subpopulation ggplotly",
  buildCdfPlot(
    spec,
    sampleSubpopData,
    fitPbtm(spec, sampleSubpopData, subpops = 2),
    1,
    interactive = TRUE
  )
)
spec <- modelSpecs$HydrothermalTime
chk(
  "HydrothermalTime ggplotly",
  buildCdfPlot(
    spec,
    sampleHydroThermData,
    fitPbtm(spec, sampleHydroThermData),
    1,
    interactive = TRUE
  )
)
spec <- modelSpecs$Hydropriming
df <- pbtm::germ_speed(samplePrimingData, 0.5, groups = spec$groups)
chk(
  "Hydropriming ggplotly",
  buildRatePlot(spec, df, fitPbtm(spec, df), interactive = TRUE)
)

cat(if (ok) "GGPLOTLY OK\n" else "GGPLOTLY FAILED\n")
if (!ok) {
  quit(status = 1)
}
