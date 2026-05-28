# Reactive-wiring check: drive the model factory with shiny::testServer (no
# browser) to confirm the req chains, fit, cache, hold/setParams observers, and
# data-summary all behave. Run from project root:
#   "C:/Program Files/R/R-4.5.3/bin/Rscript.exe" tests/reactive.R

suppressPackageStartupMessages(source("global.R"))
library(shiny)

ok <- TRUE
check <- function(label, cond) {
  cat(sprintf("  [%s] %s\n", if (isTRUE(cond)) "OK" else "FAIL", label))
  if (!isTRUE(cond)) ok <<- FALSE
}

cat("== ThermalTime (cdf) reactive flow ==\n")
testServer(
  modelServer,
  args = list(
    id = "thermalTime",
    spec = modelSpecs$ThermalTime,
    data = reactive(sampleGermData),
    ready = reactive(TRUE)
  ),
  {
    session$setInputs(
      GermTempSelect = sort(unique(sampleGermData$GermTemp)),
      dataCleanSelect = "original",
      cumFracRange = c(0, 100),
      maxCumFrac = 100
    )
    check("workingData full rows", nrow(workingData()) == nrow(sampleGermData))
    check("modelResults is list", is.list(modelResults()))
    check("lastGoodModel cached", is.list(rv$lastGoodModel))
    check("dataSummary renders", grepl("data points", output$dataSummary))

    # pin Tb via the hold checkbox -> setParams updated, fit still succeeds
    tbFit <- rv$lastGoodModel$Tb
    session$setInputs(`Tb-set` = 5)
    check("setParams captured", isTRUE(rv$heldParams$Tb) && rv$setParams$Tb == 5)
    check("fit with pinned Tb returns 5", isTRUE(all.equal(modelResults()$Tb, 5)))

    # restricting the fraction window keeps fewer rows
    session$setInputs(cumFracRange = c(10, 90))
    check("cumFracRange narrows data", nrow(workingData()) < nrow(sampleGermData))
  }
)

cat("== Hydropriming (rate) reactive flow ==\n")
testServer(
  modelServer,
  args = list(
    id = "hydropriming",
    spec = modelSpecs$Hydropriming,
    data = reactive(samplePrimingData),
    ready = reactive(TRUE)
  ),
  {
    session$setInputs(
      PrimingWPSelect = sort(unique(samplePrimingData$PrimingWP)),
      PrimingDurationSelect = sort(unique(samplePrimingData$PrimingDuration)),
      germSpeedBasis = 50
    )
    check("speedData has rows", nrow(speedData()) > 0)
    check("GR column present", "GR" %in% names(speedData()))
    check("modelResults is list", is.list(modelResults()))
    check("lastGoodModel cached", is.list(rv$lastGoodModel))
  }
)

cat("== not-ready guard ==\n")
testServer(
  modelServer,
  args = list(
    id = "aging", spec = modelSpecs$Aging,
    data = reactive(sampleAgingData), ready = reactive(FALSE)
  ),
  {
    session$setInputs(maxCumFrac = 100, cumFracRange = c(0, 100), dataCleanSelect = "original")
    # workingData req(ready()) should silently block -> NULL result
    wd <- tryCatch(workingData(), error = function(e) "blocked")
    check("workingData blocked when not ready", is.null(wd) || identical(wd, "blocked"))
  }
)

cat(sprintf("\n%s\n", strrep("-", 40)))
if (ok) cat("REACTIVE CHECKS PASSED ✓\n") else { cat("REACTIVE CHECKS FAILED\n"); quit(status = 1) }
