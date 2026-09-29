# Subpopulation mixtures through the app: the fitting itself is tested in the
# pbtm package; this checks the app's adapter, results table, and plot handle
# mixture fits. Uses the two-subpopulation thermal time sample dataset. Run from
# project root:
#   "C:/Program Files/R/R-4.6.1/bin/Rscript.exe" tests/mixture.R

suppressPackageStartupMessages(source("global.R"))
library(shiny)

ok <- TRUE
chk <- function(lbl, cond) {
  cat(sprintf("  [%s] %s\n", if (isTRUE(cond)) "OK" else "FAIL", lbl))
  if (!isTRUE(cond)) ok <<- FALSE
}

spec <- modelSpecs$ThermalTime
df <- sample_data$thermal_time_subpop$data

single <- fitPbtm(spec, df)
mix <- fitPbtm(spec, df, subpops = 2)
auto <- fitPbtm(spec, df, subpops = "auto")

cat("AIC comparison:\n")
print(auto$subpop_table)

# NOTE: PBT subpopulation mixtures are frequently *equifinal* — quite different
# parameter sets fit the same time course nearly identically (see
# md/10-subpopulations.md). So we check that mixtures work and improve the fit,
# not exact parameter recovery.
chk("k=2 fits", inherits(mix, "pbtm_fit") && mix$k == 2)
chk("mixture improves on single fit", mix$stats$aic < single$stats$aic)
chk("auto-detect picks 2 subpopulations", auto$k == 2)
chk("auto comparison table present", is.data.frame(auto$subpop_table))
chk("weights sum to 1", abs(sum(mix$components$weight) - 1) < 1e-8)

# results table and plot render for a mixture
ui <- mixtureResultsWell(spec, auto)
chk("mixture results well renders", inherits(ui, "shiny.tag.list"))
p <- buildCdfPlot(spec, df, mix, 1)
chk(
  "mixture plot builds",
  !inherits(try(ggplot2::ggplot_build(p), silent = TRUE), "try-error")
)
curve <- buildCdfCurveData(spec, df, mix)
chk("mixture curve predictions in [0, 1]", all(curve$pred >= 0 & curve$pred <= 1))

cat(sprintf("\n%s\n", strrep("-", 40)))
if (ok) {
  cat("MIXTURE CHECKS PASSED ✓\n")
} else {
  cat("MIXTURE CHECKS FAILED\n")
  quit(status = 1)
}
