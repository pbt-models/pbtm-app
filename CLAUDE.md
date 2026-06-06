# CLAUDE.md

## Project Overview

PBTM Dashboard — an R Shiny web app for population-based threshold models of seed germination. Users upload germination data, validate columns, then fit nonlinear least-squares models (thermal time, hydrotime, etc.) and visualize results.

## Running the App

```r
# Restore packages from lockfile (first time or after pulling)
renv::restore()

# Run the app
shiny::runApp()
```

There is no test suite or linter configured.

## Architecture

**Entry points:** `global.r` → `ui.r` → `server.r`. `global.R` sources `src/` in an explicit order: the core library files (`helpers.R`, `fit.R`, `ui-components.R`, `plot-helpers.R`) first, then everything else (`model-specs.R`, `model-module.R`, and the `tab_01/02/03` modules) automatically.

**UI framework:** bslib (Bootstrap 5) with `page_sidebar()` + `navset_hidden()`. Navigation uses `actionLink` in the sidebar with `observeEvent` handlers calling `nav_select("mainNav", tabId)`.

**Two module styles:**
- **Bespoke tabs** — Intro, Load data, and Germination each define `{Name}UI()`/`{Name}Server()` in `src/tab_01/02/03_*.R`. These are genuinely different from each other and from the nls models.
- **Config-driven model factory** — the 8 nls models (thermal time, hydrotime, hydrothermal time, aging, promoter, inhibitor, hydropriming, hydrothermal priming) are **not** separate files. Each is a *spec* in `src/model-specs.R`; `src/model-module.R` provides `modelUI(spec)` and `modelServer(spec, data, ready)` that generate the tab from the spec. To change a model, edit its spec; to add one, add a spec.

**Wiring** (`ui.r`/`server.r`): iterate `c("Intro","LoadData", modelNames)`; if the name is in `modelSpecs` use the factory (`modelUI`/`modelServer`), otherwise call the bespoke `{Name}UI`/`{Name}Server`. `modelServer`'s first formal is `id` (defaults to `spec$id`) so `shiny::testServer` recognises it as a module server — **call it by name in production** (`modelServer(spec = ..., ...)`).

**Model names / specs** are derived from `data/column-validation.csv` column headers (Germination through Inhibitor). This CSV drives column validation, per-model column requirements, and the spec list names (`modelSpecs[[modelCol]]`).

**Model families** (`spec$family`):
- `"cdf"` — thermal/hydro/hydrothermal time, aging, promoter, inhibitor. Fit `CumFraction ~ maxFrac * pnorm(z)` directly; plot cumulative germination vs. time with one fitted curve per factor level. Promoter/inhibitor add a log/none dosage transform; aging/inhibitor use `lower.tail = FALSE`.
- `"rate"` — hydropriming, hydrothermal priming. First reduce to a germination-rate table (`addFracDiff` + `interpolateGermSpeed`), then fit `GR ~ linear(theta)`; plot GR vs. theta with an abline.

**Single source of truth:** each spec's `predict(data, p, maxFrac, transform)` is reused by the nls fit, the fitted-curve overlay (`buildCdfCurveData`), the pseudo-R², and the subpopulation mixture. No formula is written more than once.

**Subpopulations** (`src/fit-mixture.R`, CDF specs only, `spec$subpop = TRUE`): a generic mixture-of-distributions layer. `mixturePredict` is a weighted sum of `spec$predict` over k components (each with its own param copy); weights use stick-breaking (`w1..w{k-1}` each bounded in (0,1)). `fitMixture` multi-starts and keeps the best AIC; `detectSubpops` compares k=1..3. The factory exposes a per-tab "Subpopulations: 1/2/3/Auto" control; k=1 is the ordinary single fit. `spec$subpopParam` names the threshold parameter the fitter spreads components along. Note: these mixtures are often **equifinal** (see `md/subpopulations.md`) — the tool improves fit and flags multiplicity but does not guarantee unique parameter recovery. `getModelCoefs` uses `coef()` (not `summary()$coefficients`) so singular mixture Hessians don't break coefficient extraction.

**Fitting core** (`src/fit.R`): pure functions, no `parent.frame()` mutation.
1. `resolveParams(setParams, paramRanges)` → `list(lower, start, upper, fixed)`; a user-pinned param has `lower == start == upper`.
2. `fitModel(pred, data, resolved, response)` → builds a generic `nls` (`algorithm = "port"`, `warnOnly`) from a model's `predict`; returns a results list **or an error string** (the modelResults contract: list = success, string = error).
3. `buildResults()` → coefficients (pinned values win) plus `PseudoR2 = cor(obs, pred)^2` (displayed as "Pseudo-R²").
4. `rv$lastGoodModel` caches the last successful fit; on failure the table/plot keep showing it and `modelErrorUI` notes the coefficients are the last valid ones (no auto-clear on data change).

**Shared UI components** (`src/ui-components.R`): `ns`-taking builders called inside `renderUI`: `germSlidersUI`, `germSpeedSliderUI`, `setParamsUI`, `modelResultsUI` (takes `paramNames` to decide which rows get hold-checkboxes), `modelErrorUI`, `trtSelectUI`/`trtSelectServer`, `dataCleanUI`, `dataTransfUI`, `namedWell`, `primaryBox`.

**Plot helpers** (`src/plot-helpers.R`): `buildCdfCurveData` (predicted curves as real data over a grid — `geom_line`, not `stat_function`, so they survive `ggplotly`), `buildCdfPlot`, `buildRatePlot`, `addFracToPlot`, `addParamsToPlot` (static-only plotmath annotations).

**Data flow:** `LoadDataServer()` returns a reactive list with `data`, `colStatus`, and `modelReady`. These are stored in a top-level `reactiveValues` in `server.r` and passed to each model server as `data` and `ready` reactives.

**Tests** (`tests/`, run with `Rscript`): `equivalence.R` (generic fit reproduces the original literal nls formulas on every sample dataset), `runtime.R` (all 8 plots build via `ggplot_build`), `reactive.R` (factory reactive flow via `testServer`). No browser needed.

## Key Conventions

- **Indentation:** 2 spaces (set in `.Rproj`)
- **UI wrappers:** `primaryBox()` wraps `card()` in `column()`. `namedWell()` creates titled well panels using `div(class = "p-3 bg-light border rounded")`.
- **Badges:** Rendered via `renderUI` returning `span(class = "badge bg-{success|warning|danger}")`.
- **Global helpers:** `truthy()` for content-aware truthiness checks, `parseSpeeds()` for germination speed input parsing, `getColChoices()` for labeled factor levels.
- **Package management:** `renv` — use `renv::snapshot()` after adding/updating packages.
