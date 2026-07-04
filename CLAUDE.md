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

There is no linter configured. There is a headless test suite in `tests/` (see the Tests section below) — no browser needed.

## Architecture

**Entry points:** `global.R` → `ui.R` → `server.R`. `global.R` sources every `src/**/*.R` file with a single `list.files("src", pattern = "\\.R$", recursive = TRUE, full.names = TRUE) |> lapply(source)` — there's no manual ordering; keep files free of load-order dependencies (e.g. don't call another `src/` file's function at source time, only inside functions/reactives).

**UI framework:** bslib (Bootstrap 5), `page(theme = app_theme, ...)` (see `src/theme.R`). Main navigation is a single `navset_pill_list(id = "mainNav", ...)` in `ui.R`, with per-tab status badges rendered via `uiOutput` in each `nav_panel`'s title (`renderUI` → `span(class = "badge bg-{success|warning|danger}")` in `server.R`). Tab switching from within a tab still goes through `nav_select("mainNav", tabId)` via `observeEvent` in `server.R`.

**Two module styles:**
- **Bespoke tabs** — Load data (`src/modules/load_data.R`) and Germination (`src/modules/germination.R`) each define `{name}UI()`/`{Name}Server()`. These are genuinely different from each other and from the nls models. The About tab has no server; it's static `renderMd()` output assembled directly in `ui.R`.
- **Config-driven model factory** — the 8 nls models (thermal time, hydrotime, hydrothermal time, aging, promoter, inhibitor, hydropriming, hydrothermal priming) are **not** separate files. Each is a *spec* in `src/model_specs.R`; `src/modules/models.R` provides `modelUI(spec)` and `modelServer(spec, data, ready)` that generate the tab from the spec. To change a model, edit its spec; to add one, add a spec.

**Wiring** (`ui.R`/`server.R`): iterate `c("LoadData", modelNames)` (`modelNames` includes `"Germination"`); if the name is in `modelSpecs` use the factory (`modelUI`/`modelServer`), otherwise call the bespoke `{Name}UI`/`{Name}Server`. `modelServer`'s first formal is `id` (defaults to `spec$id`) so `shiny::testServer` recognises it as a module server — **call it by name in production** (`modelServer(spec = ..., ...)`).

**Docs / "About" links:** per-model documentation (`md/*.md`) is shown two ways: (1) a modal popup opened from a `tabHeader()` link — `build_modal_link(doc, doc_label)` in `src/docs.R` sets a `show_modal` input, and `server.R` calls `show_modal(md = ...)` which renders the markdown into a `modalDialog`; (2) the full text of every model's doc is also concatenated on the **About** tab (`ui.R`, first `nav_panel`) via `renderMd()`, wrapped in `div(class = "prose-doc", ...)` for a readable line length. `renderMd()` caches rendered HTML per file path.

**Layout grammar (model tabs + Germination):** both use bslib `layout_sidebar()`. Controls live in `sidebar()` as an `accordion()` of `accordion_panel()`s, each built from `controlSection(title = ..., ...)` blocks (a flat labeled group — replaced the old nested grey wells). Outputs live in the main area as `panelCard()`s (a quiet `card()`/`card_header()`/`card_body()` wrapper that replaced `primaryBox()`; `primaryBox()` has been removed — it had no remaining call sites). Each tab opens with `tabHeader(title, subtitle, doc, doc_label)` for the title/subtitle/"About" modal link. The plot card's header carries the Static/Interactive toggle via `plotModeToggle(ns)` (passed as `panelCard(..., tools = plotModeToggle(ns))`). `namedWell()` (a titled `div(class = "p-3 bg-light border rounded")`) is still used for the results wells (`singleResultsWell`/`mixtureResultsWell` in `src/modules/models.R`) — it was intentionally kept, not retired.

**Model names / specs** are derived from `data/column-validation.csv` column headers (Germination through Inhibitor). This CSV drives column validation, per-model column requirements, and the spec list names (`modelSpecs[[modelCol]]`).

**Model families** (`spec$family`):
- `"cdf"` — thermal/hydro/hydrothermal time, aging, promoter, inhibitor. Fit `CumFraction ~ maxFrac * pnorm(z)` directly; plot cumulative germination vs. time with one fitted curve per factor level. Promoter/inhibitor add a log/none dosage transform; aging/inhibitor use `lower.tail = FALSE`.
- `"rate"` — hydropriming, hydrothermal priming. First reduce to a germination-rate table (`addFracDiff` + `interpolateGermSpeed`), then fit `GR ~ linear(theta)`; plot GR vs. theta with an abline.

**Single source of truth:** each spec's `predict(data, p, maxFrac, transform)` is reused by the nls fit, the fitted-curve overlay (`buildCdfCurveData`), the pseudo-R², and the subpopulation mixture. No formula is written more than once.

**Subpopulations** (`src/fit_subpop.R`, CDF specs only, `spec$subpop = TRUE`): a generic mixture-of-distributions layer. `mixturePredict` is a weighted sum of `spec$predict` over k components (each with its own param copy); weights use stick-breaking (`w1..w{k-1}` each bounded in (0,1)). `fitMixture` multi-starts and keeps the best AIC; `detectSubpops` compares k=1..3. The factory exposes a per-tab "Subpopulations: 1/2/3/Auto" control; k=1 is the ordinary single fit. `spec$subpopParam` names the threshold parameter the fitter spreads components along. Note: these mixtures are often **equifinal** (see `md/subpopulations.md`) — the tool improves fit and flags multiplicity but does not guarantee unique parameter recovery. `getModelCoefs` uses `coef()` (not `summary()$coefficients`) so singular mixture Hessians don't break coefficient extraction.

**Fitting core** (`src/fit_model.R`): pure functions, no `parent.frame()` mutation.
1. `resolveParams(setParams, paramRanges)` → `list(lower, start, upper, fixed)`; a user-pinned param has `lower == start == upper`.
2. `fitModel(pred, data, resolved, response)` → builds a generic `nls` (`algorithm = "port"`, `warnOnly`) from a model's `predict`; returns a results list **or an error string** (the modelResults contract: list = success, string = error).
3. `buildResults()` → coefficients (pinned values win) plus `PseudoR2 = cor(obs, pred)^2` (displayed as "Pseudo-R²").
4. `rv$lastGoodModel` caches the last successful fit; on failure the table/plot keep showing it and `modelErrorUI` notes the coefficients are the last valid ones (no auto-clear on data change).

**Shared UI components** (`src/modules/models.R`, plus `src/modules/__trt_select.R` for treatment filters): `ns`-taking builders called inside `renderUI`: `germSlidersUI`, `germSpeedSliderUI`, `setParamsUI`, `singleResultsWell`/`mixtureResultsWell` (the single-fit vs. mixture results tables — `singleResultsWell` takes `paramNames` to decide which rows get hold-checkboxes), `modelErrorUI`, `trtSelectUI`/`trtSelectServer`, `dataCleanUI`, `dataTransfUI`. `namedWell()` and the newer container helpers (`panelCard`, `controlSection`, `tabHeader`, `plotModeToggle`) live in `global.R`.

**Plot helpers** (`src/plot_helpers.R`): `buildCdfCurveData` (predicted curves as real data over a grid — `geom_line`, not `stat_function`, so they survive `ggplotly`), `buildCdfPlot`, `buildRatePlot`, `addFracToPlot`, `addParamsToPlot` (static-only plotmath annotations).

**Data flow:** `loadDataServer()` (`src/modules/load_data.R`) returns a reactive list with `data`, `colStatus`, and `modelReady`. These are stored in a top-level `reactiveValues` in `server.R` and passed to each model server as `data` and `ready` reactives.

**Tests** (`tests/`, run with `Rscript`): `equivalence.R` (generic fit reproduces the original literal nls formulas on every sample dataset), `runtime.R` (all 8 plots build via `ggplot_build`), `plotly.R` (all 8 plots also build via `ggplotly()`), `mixture.R` (subpopulation mixture fitting/AIC comparison), `reactive.R` (factory reactive flow via `testServer`). No browser needed.

## Key Conventions

- **Indentation:** 2 spaces (set in `.Rproj`)
- **UI wrappers:** `panelCard()` (in `global.R`) is a quiet `card()`/`card_header()`/`card_body()` wrapper used for top-level output groups; it replaced `primaryBox()` (retired — no call sites remain). `controlSection()` is a flat labeled block for grouping sidebar controls. `tabHeader()` renders a tab's title + subtitle + optional "About" modal link. `namedWell()` creates titled well panels using `div(class = "p-3 bg-light border rounded")` and remains in use for the model results wells.
- **Badges:** Rendered via `renderUI` returning `span(class = "badge bg-{success|warning|danger}")`.
- **Global helpers:** `truthy()` for content-aware truthiness checks, `parseSpeeds()` for germination speed input parsing, `getColChoices()` for labeled factor levels.
- **Package management:** `renv` — use `renv::snapshot()` after adding/updating packages.
