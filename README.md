# urbanafrica

Measuring urban built-up expansion across the **100 largest African agglomerations**
(by Africapolis 2020 population), 2000–2025, and characterising *how* they grew —
sprawl vs. densification, lit (formal) vs. unlit (informal) settlement.

Two satellite inputs:

- **GHSL-BUILT-S R2023** — built-up surface, m² per 100 m cell, six epochs
  (2000, 2005, 2010, 2015, 2020, 2025), total and non-residential layers.
- **Nighttime lights** — harmonised DMSP-OLS + VIIRS annual series 2000–2024, via
  Google Earth Engine; used to classify each built-up pixel lit / unlit.

The main product is a **Shiny app** (`app/`) with interactive growth maps, NTL /
lit-unlit maps, multi-city time series, a rankings table, and sprawl scatterplots.

## Repository layout

```
scripts/               pipeline scripts — run from the repo root: Rscript scripts/<name>.R
scripts/exploratory/   one-off / prototype scripts, not part of the pipeline
app/                   the Shiny app (app.R + a small app/data/ bundle)
data/                  source + derived data — NOT in git (see below)
output/                generated plots / maps — NOT in git
CLAUDE.md              detailed pipeline, data architecture, conventions, planned work
```

## Data

The `data/` tree (~100 GB of rasters) is **not** version-controlled. Source data must
be downloaded manually:

- **GHSL** (BUILT-S, and optionally POP / BUILT-V): <https://human-settlement.emergency.copernicus.eu/download.php>
- **Africapolis 2020** agglomeration boundaries: <https://africapolis.org>
- **Nighttime lights** via `scripts/0_loadntl.R` (needs an Earth Engine account).

See the *Data architecture* and *Pipeline & run order* sections of
[`CLAUDE.md`](CLAUDE.md) for exact paths and the order scripts must run in.

The five small `.Rds` files in `app/data/` **are** tracked, so the app can run
without regenerating the full pipeline.

## Running the app

```r
# from the repo root
shiny::runApp("app", launch.browser = TRUE)
```

Package dependencies load via `pacman::p_load(...)` at the top of each script.

## Environment variables

`scripts/0_loadntl.R` and `scripts/exploratory/2_3_rgee.R` read (from `~/.Renviron`):

- `RGEE_PYTHON` — path to a Python env with `earthengine-api`
- `EE_USER` — your Earth Engine account email

## Status

Work in progress. Planned additions (GHS-POP population-over-time, GHS-BUILT-V
vertical growth, a footprint-over-time slider) are tracked in
[`CLAUDE.md`](CLAUDE.md) under *Planned work*.
