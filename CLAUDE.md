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

**All model math comes from the `pbtm` package** (github.com/pbt-models/pbtm, source in `../pbtm-package`; installed into renv from GitHub). The app has no formulas, nls code, or mixture code of its own. Each spec names its pbtm model (`spec$pbtm`, e.g. `"thermal_time"`); `modelSpec()` pulls `family`, `factors`, `paramNames`, `groups`, `transformCol`, and (rate models) `theta()` from `pbtm::pbtm_models(id)`. A spec itself only adds UI config: labels, `annotate()` plotmath, plot styling, docs. Always call pbtm with `pbtm::` (the package is not attached).

**Model families** (`spec$family`, from pbtm):
- `"cdf"` — thermal/hydro/hydrothermal time, aging, promoter, inhibitor. Fit to the (filtered, optionally `pbtm::clean_germ_data()`-cleaned) time courses; plot cumulative germination vs. time with one fitted curve per factor level. Promoter/inhibitor take a log/none dosage transform.
- `"rate"` — hydropriming, hydrothermal priming. `speedData()` is `pbtm::germ_speed()` at the chosen fraction (a table with `Fraction`, `Time`, `GR`); the fit uses that table and the plot draws GR vs. `spec$theta()` with the fitted line.

**Fitting** (`src/fit_pbtm.R`): `fitPbtm(spec, data, maxFrac, logDose, subpops, fixed)` wraps `pbtm::fit_pbtm()` and keeps the app's **modelResults contract: a `pbtm_fit` (a list) = success, a character string = error**. pbtm's warnings (non-convergence, an estimate on its bound) are captured into `fit$warnings` and shown by `modelErrorUI` as a "Check this fit" notice rather than being swallowed. Read results through the fit object, never through old-style flat fields: `coef(fit)` for parameters, `fit$stats$pseudo_r2` / `$aic`, `fit$k`, `fit$components` (one row per subpopulation), `fit$subpop_table` (auto-detect comparison), `predict(fit, newdata)` for curves. `fitValues(fit)` flattens coefficients + `PseudoR2` for the specs' `annotate()`/`theta()`. User-pinned params (`rv$setParams`) are passed as pbtm's `fixed`. `rv$lastGoodModel` caches the last successful fit; on failure the table/plot keep showing it and `modelErrorUI` notes the coefficients are the last valid ones (no auto-clear on data change).

**Subpopulations** (CDF specs only, `spec$subpop`): a per-tab "Subpopulations: 1/2/3/Auto" control maps to pbtm's `subpops` (1, 2, 3, or `"auto"`, which compares k = 1..3 by AIC). Pinned params apply only to k = 1. Mixtures are often **equifinal** (see `md/10-subpopulations.md`): they improve fit and flag multiplicity but don't guarantee unique parameter recovery.

**Shared UI components** (`src/modules/models.R`, plus `src/modules/__trt_select.R` for treatment filters): `ns`-taking builders called inside `renderUI`: `germSlidersUI`, `germSpeedSliderUI`, `setParamsUI`, `singleResultsWell`/`mixtureResultsWell` (the single-fit vs. mixture results tables — `singleResultsWell` takes `paramNames` to decide which rows get hold-checkboxes), `modelErrorUI`, `trtSelectUI`/`trtSelectServer`, `dataCleanUI`, `dataTransfUI`. `namedWell()` and the newer container helpers (`panelCard`, `controlSection`, `tabHeader`, `plotModeToggle`) live in `global.R`.

**Plot helpers** (`src/plot_helpers.R`): `buildCdfCurveData` (predicted curves from `predict(fit, grid)` as real data — `geom_line`, not `stat_function`, so they survive `ggplotly`; works for single fits and mixtures alike), `buildCdfPlot`, `buildRatePlot`, `addFracToPlot`, `addParamsToPlot` (static-only plotmath annotations). The Germination tab's speed table/markers and "rescale" option use `pbtm::germ_speed()` and `pbtm::rescale_cum_frac()`.

**Data flow:** `loadDataServer()` (`src/modules/load_data.R`) returns a reactive list with `data`, `colStatus`, and `modelReady`. These are stored in a top-level `reactiveValues` in `server.R` and passed to each model server as `data` and `ready` reactives.

**Tests** (`tests/`, run with `Rscript`): `runtime.R` (spec params match pbtm; all 8 models fit on their sample dataset and their plots build via `ggplot_build`; rate plots must draw the fitted line; fit warnings/errors come back as data, not crashes), `plotly.R` (single, mixture, and rate plots also build via `ggplotly()`), `mixture.R` (mixture fits flow through the adapter, results table, and plot), `reactive.R` (factory and Germination reactive flow via `testServer`). No browser needed. The model math itself is tested in the pbtm package, including regression tests against the estimates this app produced before it switched to pbtm (`tests/testthat/test-fit-reference.R`). `.dev/equivalence.R` is a retired one-off from the earlier factory refactor and no longer runs.

**Updating pbtm:** `renv::install("pbt-models/pbtm@<branch or tag>")` then `renv::snapshot()`. After a pbtm change that renames parameters or result fields, run `tests/runtime.R` first: its "params match pbtm" checks catch spec/annotation drift.

## Key Conventions

- **Indentation:** 2 spaces (set in `.Rproj`)
- **Model parameter names:** lowercase snake_case `symbol_subscript`, shared with the `pbtm` package: `t_b`, `theta_t50`, `sigma` (thermal time); `theta_h`, `psi_b50` (hydrotime); `theta_ht`, `t_b`, `psi_b50` (hydrothermal time); `psi_min`, `t_min`, `gr_i`, `slope` (priming); `theta_a`, `p_max50` (aging); `theta_p`, `p_b50` (promoter); `theta_i`, `i_b50` (inhibitor). These are defined by pbtm. Read them with `coef(fit)[["name"]]` or `fitValues(fit)$name` — never `fit$name`, which silently returns `NULL` (a stale lookup like that once hid the priming fit line).
- **No rounding of estimates:** results are kept at full precision; round only for display (`signif()` in the results tables).
- **UI wrappers:** `panelCard()` (in `global.R`) is a quiet `card()`/`card_header()`/`card_body()` wrapper used for top-level output groups; it replaced `primaryBox()` (retired — no call sites remain). `controlSection()` is a flat labeled block for grouping sidebar controls. `tabHeader()` renders a tab's title + subtitle + optional "About" modal link. `namedWell()` creates titled well panels using `div(class = "p-3 bg-light border rounded")` and remains in use for the model results wells.
- **Badges:** Rendered via `renderUI` returning `span(class = "badge bg-{success|warning|danger}")`.
- **Global helpers:** `truthy()` for content-aware truthiness checks, `parseSpeeds()` for germination speed input parsing, `getColChoices()` for labeled factor levels.
- **Package management:** `renv` — use `renv::snapshot()` after adding/updating packages.
