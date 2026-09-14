# Extract NTL for Africapolis Shapefile
# ── 1. Setup ──────────────────────────────────────────────────────────────────
pacman::p_load(
  tidyverse, terra, sf, here,
  ggplot2, scales, janitor,
  leaflet, leaflet.extras, htmlwidgets,
  raster  # needed only for leaflet::addRasterImage()
)

# 2. Canonical top-100 agglomerations -----------------------------------------
# Repaired geometry + Kisumu excluded; see scripts/0_simplifyshapefile.R.
agglom_100 <- st_read(here("data/intermediate/agglom_top100.gpkg"), quiet = TRUE)

# 3. Raster ---------------------------------------------------------------
# Bloom- + top-coding-corrected DMSP series (Chiovelli, Michalopoulos,
# Papaioannou & Regan 2026, "Illuminating the Global South"). Annual 1992-2025,
# ~1 km, layer names are years. Values are corrected/extended DN (0..~2000),
# NOT nW/cm2/sr. See scripts/0_ntlprep_bltcfix.R.
ntl_total <- terra::rast(here("data/intermediate/raster/ntl_bltcfix_africa.tif"))

# 4. Extract per-city NTL ------------------------------------------------------
agglom_vect <- vect(agglom_100) |> project(crs(ntl_total))

ntl_by_city <- function(stack) {
  meta <- tibble(id         = agglom_100$id,
                 agglosname = agglom_100$agglos_name,
                 iso3       = agglom_100$iso3)

  extract_stat <- function(fun, varname) {
    terra::extract(stack, agglom_vect, fun = fun, na.rm = TRUE, ID = FALSE) |>
      as_tibble() |>
      bind_cols(meta) |>
      pivot_longer(cols = -c(id, agglosname, iso3),
                   names_to = "year", values_to = varname) |>
      mutate(year = as.integer(year))
  }

  # Lit = any detected light. The paper's own lit/unlit convention is DN > 0;
  # the blooming correction is what makes DN > 0 a valid cut (the dim bleed a
  # 0.5 nW threshold used to filter is already removed). On this corrected
  # series lit share saturates (~1.0 for large agglomerations by 2025), so we
  # also carry a "dim" share: 0 < DN <= DIM_MAX. DIM_MAX = 10 sits in the DMSP
  # "marginal light" range (native DN 0-63); see 1_create_citydata.R.
  DIM_MAX <- 10
  keys <- c("id", "agglosname", "iso3", "year")
  extract_stat(mean,                                    "ntl_mean") |>
    left_join(extract_stat(sum,                         "ntl_sum"),       by = keys) |>
    left_join(extract_stat(\(x, na.rm = TRUE) mean(x > 0, na.rm = na.rm),               "ntl_lit_share"),    by = keys) |>
    left_join(extract_stat(\(x, na.rm = TRUE) mean(x > 0 & x <= DIM_MAX, na.rm = na.rm), "ntl_dimlit_share"), by = keys) |>
    dplyr::select(all_of(keys), ntl_mean, ntl_sum, ntl_lit_share, ntl_dimlit_share)
}

agglom_ntl <- ntl_by_city(ntl_total)

# 5. Save Output ----------------------------------------------------------
saveRDS(agglom_ntl, here("data/intermediate/africapolis_ntl.Rds"))
