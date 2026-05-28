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

**Entry points:** `global.r` → `ui.r` → `server.r`. All files in `src/` are auto-sourced by `global.r` via `list.files("src", "*.R", full.names = TRUE) |> lapply(source)`.

**UI framework:** bslib (Bootstrap 5) with `page_sidebar()` + `navset_hidden()`. Navigation uses `actionLink` in the sidebar with `observeEvent` handlers calling `nav_select("mainNav", tabId)`.

**Module pattern:** Each tab defines a `{Name}UI()` function and `{Name}Server()` function in the same file. Model tabs follow the naming convention `tab_NN_modelName.R`. The UI and server are wired dynamically in `ui.r` and `server.r`:
- UI: `exec(paste0(m, "UI"))` creates each tab panel
- Server: `do.call(paste0(m, "Server"), list(data = ..., ready = ...))` calls each model server

**Model names** are derived from `data/column-validation.csv` column headers (Germination through Inhibitor). This CSV also drives column validation rules and per-model column requirements.

**Shared UI components** (`global.R`): Reusable UI builders that take a `ns` (namespace function) parameter and are called within model tab `renderUI` blocks. Key ones:
- `germSlidersUI` — max germination and fraction range sliders
- `setParamsUI` — optional coefficient inputs
- `modelResultsUI` — displays fitted coefficients with hold checkboxes
- `modelErrorUI` — shows error when model fails to converge
- `trtSelectUI`/`trtSelectServer` — additional treatment filtering
- `dataCleanUI`, `dataTransfUI`, `germSpeedSliderUI`

**Data flow:** `LoadDataServer()` returns a reactive list with `data`, `colStatus`, and `modelReady`. These are stored in a top-level `reactiveValues` in `server.r` and passed to each model server as `data` and `ready` reactives.

**Model execution pattern** (consistent across all model tabs):
1. `paramRangeDefaults` — defines (lower, start, upper) bounds for NLS parameters
2. `workingData` reactive — filters data by treatment selections and fraction range
3. `buildModelParams()` — separates user-fixed vs. free parameters
4. `nls()` with `algorithm = "port"` — fits the model
5. `buildModelResults()` — extracts coefficients and R²
6. `rv$lastGoodModel` — caches last successful fit for display when model fails

## Key Conventions

- **Indentation:** 2 spaces (set in `.Rproj`)
- **UI wrappers:** `primaryBox()` wraps `card()` in `column()`. `namedWell()` creates titled well panels using `div(class = "p-3 bg-light border rounded")`.
- **Badges:** Rendered via `renderUI` returning `span(class = "badge bg-{success|warning|danger}")`.
- **Global helpers:** `truthy()` for content-aware truthiness checks, `parseSpeeds()` for germination speed input parsing, `getColChoices()` for labeled factor levels.
- **Package management:** `renv` — use `renv::snapshot()` after adding/updating packages.
