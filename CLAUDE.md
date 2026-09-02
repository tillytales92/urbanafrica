# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Purpose

**urbanafrica** measures urban built-up expansion across the **100 largest African
agglomerations** (by Africapolis 2020 population) from 2000 to 2025, and characterises
*how* they grew — sprawl vs. densification, lit (formal) vs. unlit (informal) settlement.

Two satellite inputs:
- **GHSL-BUILT-S R2023** (Global Human Settlement Layer) — built-up surface, m² per
  100 m pixel, six epochs (2000, 2005, 2010, 2015, 2020, 2025), separate total and
  non-residential layers (residential = total − non-residential). Native Mollweide,
  EPSG:54009.
- **Nighttime lights (NTL)** — harmonised DMSP-OLS + VIIRS annual series 2000–2024,
  pulled from Google Earth Engine. Used to classify each built-up pixel lit / unlit
  at a **0.5 nW/cm²/sr** threshold. 2025 has no NTL — 2024 is used as a proxy.

Primary deliverable: the **Shiny app** in `app/` (interactive growth maps, NTL / lit-unlit
maps, multi-city time series, rankings table, sprawl scatterplots). Secondary: static
ggplot time-series PNGs and per-metro leaflet HTML in `output/`.

GHSL pixel values are 0–10,000 (m² built-up per 100 m × 100 m cell). Sum pixels inside a
polygon and divide by 1,000,000 to get km². Do all raster maths in the native equal-area
Mollweide CRS; reproject to EPSG:4326 only for leaflet display.

## Repository layout

```
scripts/               active pipeline scripts (run from the project root: Rscript scripts/<name>.R)
scripts/exploratory/   one-off / prototype scripts not part of the pipeline
app/                   the Shiny app (app.R + app/data/)
data/raw/              downloaded source data (not created by scripts)
data/intermediate/     everything the scripts derive
output/                final PNG plots and per-metro HTML maps
```

All scripts resolve paths with `here()`, which finds the project root via
`urbanafrica.Rproj` — so a script works the same wherever it sits in the tree.

## Pipeline & run order

Scripts are numbered by stage (`0_` prep → `1_` extract/derive → `2_` outputs). Within a
stage, order is not always obvious — the dependencies below matter. All paths below are
under `scripts/`.

**Stage 0 — continental prep**
- `0_ghslprep.R` — unzip, stack and crop GHSL to Africa → `data/intermediate/raster/{total,nres,res}_africa.tif` (6 bands each).
- `0_popprep.R` — unzip + name-match + crop GHS-POP to Africa → `data/intermediate/raster/pop_africa.tif` (6 bands). *(GHS-POP integration — see the Planned section; raw data not yet downloaded.)*
- `0_loadntl.R` — GEE export of the NTL stack (needs `rgee` + auth). `0_ntlprep.R` — merge the exported NTL tiles → `data/intermediate/raster/ntl_urbanafrica.tif` (25 bands, 2000–2024).
- `0_simplifyshapefile.R` — build `data/intermediate/agglom_attrs.Rds` (slug, tree-cover %, pop2020) for the top 100.

**Stage 1 — extraction & per-city assets**
- `1_extract_ghsl.R` — zonal sums of GHSL per agglomeration → `data/intermediate/africapolis_builtup.Rds`.
- `1_extract_ntl.R` — zonal NTL stats per agglomeration → `data/intermediate/africapolis_ntl.Rds`.
- `1_extract_pop.R` — zonal population sums per top-100 agglomeration → `data/intermediate/africapolis_pop.Rds`. *(GHS-POP integration — see the Planned section.)*
- `1_create_citydata.R` — cut the continental stacks into `data/intermediate/cities/<slug>/` (native + WGS84 stacks, 2000→2025 change layers, extensive-margin layers, NTL, lit/unlit tri-state). Depends on the Stage 0 continental rasters. See the script header for the full list of per-city files.
- `1_create_cityindex.R` — `data/intermediate/city_index.Rds` (centroid, bbox, Δ summary, unlit share). Depends on `africapolis_builtup.Rds` **and** a populated `cities/` directory.

**Stage 2 — outputs**
- `2_1_plots.R` — time-series PNGs → `output/`; also writes `africapolis_metro_growth.Rds`.
- `2_4_sprawl_metrics.R` — sprawl / intensification / density decomposition → `data/intermediate/sprawl_stats.Rds` (and a copy to `app/data/`).

**`scripts/exploratory/` — not wired into the app:**
- `0_ghslprep_1km.R` — continental 1 km built-up diff map (`data/raw/ghsl/1km/`), aggregation controlled by `FACT`.
- `2_2_leaflet_metro.R` — single-metro static leaflet HTML; superseded by the app. Set `target_metro` near the top; reads the per-city assets from `1_create_citydata.R`.
- `2_3_rgee.R` — Landsat SWIR-NIR-Red false-colour composites for African capitals (needs GEE).
- `test_clustering.R` — per-pixel k-means growth-trajectory typology for one city.

## Data architecture

| Path | Description |
|---|---|
| `data/raw/ghsl/GHS_BUILT_S_E<year>_GLOBE_R2023A_54009_100_V1_0.tif` | GHSL total built-up, one file per epoch |
| `data/raw/ghsl/GHS_BUILT_S_NRES_E<year>_...tif` | GHSL non-residential built-up, one file per epoch |
| `data/raw/ghsl/1km/` | 1 km GHSL variants (for `scripts/exploratory/0_ghslprep_1km.R` only) |
| `data/raw/ghsl/pop/` | GHS-POP rasters (`GHS_POP_E<year>_..._100_V1_0.tif`, 6 epochs) — **not yet downloaded** |
| `data/raw/africapolis/agglomerations.shp` | Africapolis 2020 agglomeration polygons + attributes (the canonical boundary set) |
| `data/raw/ntl/ntl_africapolis_top100_2000_2024-*.tif` | GEE-exported NTL tiles, 25 bands = years 2000–2024 |
| `data/raw/{Africa_Cities-shp,icpac_shp,ipums_shp}/` | Alternative boundary sets — currently **unused** by any script |
| `data/intermediate/raster/{total,nres,res}_africa.tif` | Continental cropped GHSL stacks (6 bands) |
| `data/intermediate/raster/ntl_urbanafrica.tif` | Merged continental NTL stack (25 bands) |
| `data/intermediate/cities/<slug>/` | Per-city cut assets consumed by the app and `scripts/exploratory/2_2_leaflet_metro.R` |
| `data/intermediate/*.Rds` | Extracted statistics and the city index |
| `app/data/` | Copies of the `.Rds` files + `cities/` symlink → `data/intermediate/cities` |
| `output/` | Final plots (PNG) and per-metro maps (HTML) |

`<slug>` is `janitor::make_clean_names(agglos_name)` throughout — the per-city directory
name and the join key between the `.Rds` tables and the `cities/` folders.

## NTL / GEE acquisition

NTL is pulled from the Earth Engine collection
`projects/sat-io/open-datasets/npp-viirs-ntl` (harmonised DMSP-OLS + VIIRS, annual
2000–2024, ~500 m) via `0_loadntl.R`. This requires a working `rgee` install plus a
Python/conda env with `earthengine-api`. `0_loadntl.R` and `scripts/exploratory/2_3_rgee.R`
read two environment variables (set them in `~/.Renviron`):

- `RGEE_PYTHON` — path to that Python env (unset → reticulate's default).
- `EE_USER` — your Earth Engine account email (unset → rgee's default credentials).

## The Shiny app (`app/app.R`)

- Reads **only** from `app/data/`: the summary `.Rds` files plus `app/data/cities`
  (a symlink to `data/intermediate/cities`).
- Launch locally: `shiny::runApp(here::here("app"), launch.browser = TRUE)`.
- Deployed to Posit Connect Cloud (`app/rsconnect/`).
- After regenerating any `.Rds` or per-city asset, copy the updated `.Rds` files into
  `app/data/` (the `cities/` symlink needs no copy).

## Planned work (not yet implemented)

### GHS-POP integration

Goal: add **GHS-POP R2023A** (residential population, persons per cell) so the app can show
population *over time* and time-varying per-capita built-up, instead of the single fixed
Africapolis `pop2020` denominator. GHS-POP shares the grid, CRS (Mollweide 54009) and
5-year epochs of the built-up stack, so it mirrors the BUILT-S flow at every stage.
Population is in an equal-area CRS, so a polygon **sum = total population** (no `/1e6`).

Status of the pieces:

| Piece | Status | Notes |
|---|---|---|
| `data/raw/ghsl/pop/GHS_POP_E<year>_..._100_V1_0.tif` | **in place** | All 6 zips (2000–2025, 100 m) in `data/raw/ghsl/pop/`; `0_popprep.R` running. |
| `0_popprep.R` | **written, not run** | Unzip + name-match by epoch + crop to Africa bbox → `data/intermediate/raster/pop_africa.tif` (6 bands). Needs the raw rasters. |
| `1_extract_pop.R` | **written, not run** | Zonal `sum` per top-100 agglomeration → `data/intermediate/africapolis_pop.Rds` (long: id, agglosname, iso3, year, pop). Sum on the **native** stack only. |
| `1_create_citydata.R` pop block | **todo** | Add `pop.tif` / `pop_wgs84.tif` / `pop_change_2000_2025_wgs84.tif` per city, gated like the urban/NTL blocks. Re-run over 100 cities (expensive). |
| `1_create_cityindex.R` | **todo** | Join pop summary; add `builtup_per_cap_2000/2025`, `pop_density_2000/2025` (pop ÷ footprint), and their deltas. |
| `2_1_plots.R` | **todo** | Population-over-time facet + built-up-per-capita plot. |
| `2_4_sprawl_metrics.R` | **todo, optional** | Population-weighted extensive/intensive margins; `pop_density_change`. |
| `app/app.R` + `app/data/` | **todo** | New "Population" map layer + time-series variable + rankings columns + scatter option; copy new `.Rds` into `app/data/`. About tab: add source + circularity caveat. |

Decisions taken for the first pass:
- **Resolution 100 m** (map-ready, matches built-up).
- **Keep Africapolis `pop2020`** for the static info row; GHS-POP drives the new time series. Do not replace yet — it would move every ranking number.
- **Epochs 2000–2025 only** (ignore GHS-POP's projected 2030) to stay aligned with BUILT-S.
- `1_extract_pop.R` uses the **top-100** selection (like `1_extract_ntl.R`), not `1_extract_ghsl.R`'s `pop2020 > 100000`. Factoring out one canonical city list is a separate cleanup.

Caveat to document in the app: GHS-POP is disaggregated *using* the built-up layer, so any
per-capita density built from GHS-POP + GHS-BUILT is partly circular.

Run order once the raw data is in place:
`0_popprep.R` → `1_extract_pop.R` → re-run `1_create_citydata.R` → `1_create_cityindex.R`
→ `2_1_plots.R` / `2_4_sprawl_metrics.R` → copy `.Rds` to `app/data/`.

### Footprint-over-time visualisation

Add a "footprint over time" view to the Urban Growth tab, alongside the current
single-year / period-change modes. Neither piece needs new source data — both use the
per-city `{res,nres,total}_wgs84.tif` stacks (bands already named by year).

| Piece | Status | Notes |
|---|---|---|
| "Footprint growth" mode with play slider (`app/app.R`) | **done (needs live-session testing; design still being tuned)** | The old single-year res/nres snapshot is replaced by an animated two-tone footprint: for the selected epoch, pixels first built **this** epoch are bright, earlier pixels pale — separately for residential (reds) and non-residential (blues), each a toggleable layer. The bright "growth ring" moves every frame so change is obvious. `first_built()` = `terra::which.lyr()` on the res / nres stacks, once per city; a slider step just re-thresholds it. Slider interval 3000 ms. `output$map` renders a bare shell once; observers push via `leafletProxy` — city (view + outline + info box), layers/legend (mode-keyed), raster (per step). "Period change" mode unchanged. Not yet exercised live — check first paint, `which.lyr`, `colorFactor`+`addRasterImage`, palette contrast on the satellite basemap. |
| Precompute `first_built_epoch_wgs84.tif` (`1_create_citydata.R`) | **todo, optional** | Move the on-the-fly `first_built()` computation into a per-city raster for speed; build it in the same 100-city re-run as the GHS-POP pop block. Only worth it if the live computation proves slow. |

Later, optional: a "Download animation" button that renders a GIF (`ggplot` + `gifski`
over the six bands) on demand for the selected city — for paper figures / sharing, not
interactive use.

## Conventions

- Use `terra` (not the deprecated `raster` package) for all raster I/O and operations;
  convert to `raster::raster()` only at the point of passing to `leaflet::addRasterImage()`.
- Match rasters/bands/columns **by name**, never by position. `0_ghslprep_1km.R` matches
  GHSL files by epoch string (`grep("E2000", ...)`); `0_ghslprep.R` still uses positional
  slices (`tifs[1:6]` / `tifs[7:12]`) and should be treated as fragile.
- Paths via `here()` only — no hardcoded absolute paths (the `rgee` scripts are the
  known exception).
- `writeRaster()` does not create parent directories — ensure `data/intermediate/raster/`
  exists before Stage 0.
- CRS: process in native Mollweide (EPSG:54009), reproject to EPSG:4326 only for leaflet.
- This repo is **not** under version control.

## Known inconsistencies (fix deliberately, don't rely on)

- `1_extract_ghsl.R` selects `pop2020 > 100000` (hundreds of cities) while
  `1_extract_ntl.R` and `1_create_citydata.R` take the top 100. Downstream joins collapse
  to the intersection, but there is no single canonical city list.
- `2_1_plots.R` subtitles say "pop2020 > 3M"; the code actually ranks the top 100 / top 16.

## Environment

This project runs on **Windows 10 with RStudio open**. Key quirks:
- RStudio autosaves but does not flush files to disk immediately — if a file reads as
  empty or stale, ask the user to save in RStudio first before reading it.
- Do not use Unix-only process tools (`pgrep`, `ps`) to monitor Windows-spawned R
  processes — they will not find them. Use PowerShell (`Get-Process`) or simply check
  output files.
- The project path contains spaces (`OneDrive - London School of Economics`) — always
  double-quote paths in Bash and PowerShell commands.

## Shell Conventions

- Always quote paths in shell commands — the project path contains spaces.
- Suppress R progress bar output when running scripts via `Rscript` to avoid flooding
  terminal output (e.g. wrap `purrr::walk` calls with `progressr` disabled, or redirect
  stderr).
- Prefer `Rscript script.R 2>&1` to capture both stdout and stderr in one stream.

## Verification

After editing or rewriting any R script, run it (or the modified section) and confirm it
executes without errors before declaring the task done. Watch for common self-introduced
bugs:
- Broken pipe chains (`|` vs `|>`)
- Missing `na.rm = TRUE` in aggregation functions
- Positional indexing of files or columns instead of name-based matching

## Scope Discipline

Only edit the scripts or sections explicitly requested. Do not add extra rasters, layers,
or extractions beyond what was asked — if something looks missing or worth adding, flag it
as a suggestion rather than implementing it unilaterally.
