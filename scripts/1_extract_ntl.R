# Extract NTL for Africapolis Shapefile
# ── 1. Setup ──────────────────────────────────────────────────────────────────
pacman::p_load(
  tidyverse, terra, sf, here,
  ggplot2, scales, janitor,
  leaflet, leaflet.extras, htmlwidgets,
  raster  # needed only for leaflet::addRasterImage()
)

# 2. Load Africapolis shapefile -------------------------------------------------
# Urban areas in Africa
agglom <- st_read(here("data/raw/africapolis/agglomerations.shp")) |>
  clean_names()

#Urban areas in Africa: largest 100
agglom_100 <- agglom |>
  slice_max(pop2020, n = 100) |>   # TEMP: small sample for testing
  st_make_valid()

# 3. Raster ---------------------------------------------------------------
ntl_total <- terra::rast(here("data/intermediate/raster/ntl_urbanafrica.tif"))

# 4. Extract per-city NTL ------------------------------------------------------
# Project polygons into NTL raster CRS for extraction.
# Harmonised DMSP-OLS + VIIRS series (2000–2024); layer names are years.
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

  keys <- c("id", "agglosname", "iso3", "year")
  extract_stat(mean,                                    "ntl_mean") |>
    left_join(extract_stat(sum,                         "ntl_sum"),       by = keys) |>
    left_join(extract_stat(\(x, na.rm = TRUE) mean(x > 0.5, na.rm = na.rm), "ntl_lit_share"), by = keys) |>
    dplyr::select(all_of(keys), ntl_mean, ntl_sum, ntl_lit_share)
}

agglom_ntl <- ntl_by_city(ntl_total)

# 5. Save Output ----------------------------------------------------------
saveRDS(agglom_ntl, here("data/intermediate/africapolis_ntl.Rds"))
