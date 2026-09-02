# Extract GHSL for Africapolis Shapefile
# ── 1. Setup ──────────────────────────────────────────────────────────────────
pacman::p_load(
  tidyverse, terra, sf, here,
  ggplot2, scales, janitor,
  leaflet, leaflet.extras, htmlwidgets,
  raster  # needed only for leaflet::addRasterImage()
)

dir.create(here("output"),            showWarnings = FALSE)
dir.create(here("data/intermediate"), showWarnings = FALSE)

target_years <- c(2000, 2005, 2010, 2015, 2020, 2025)

# 2. Load Africapolis shapefile -------------------------------------------------
# Urban areas in Africa
agglom <- st_read(here("data/raw/africapolis/agglomerations.shp")) |>
  clean_names()

#for testing: select larger than 100,000
agglom_sel <- agglom |>
  filter(pop2020 > 100000)

# 3. Rasters ---------------------------------------------------------------
####GHSL####
#TOTAL
ghsl_total <- terra::rast(here("data/intermediate/raster/total_africa.tif"))
#NRES
ghsl_nres <- terra::rast(here("data/intermediate/raster/nres_africa.tif"))
names(ghsl_nres) <- c(2000,2005,2010,2015,2020,2025)

# 4. Extract per-city built-up area --------------------------------------------
# Project polygons into GHSL native CRS (Mollweide / EPSG:54009) for extraction.
# GHSL-BUILT-S pixel value = m² of built-up surface within a 100 m × 100 m cell
# (range 0–10,000). Summing pixel values inside a polygon gives the total
# built-up surface (m²) for that polygon; ÷ 1e6 → km².
agglom_vect <- vect(agglom_sel) |> project(crs(ghsl_total))

sum_by_city <- function(stack, label) {
  terra::extract(stack, agglom_vect, fun = sum, na.rm = TRUE, ID = FALSE) |>
    as_tibble() |>
    mutate(id         = agglom_sel$id,
           agglosname = agglom_sel$agglos_name,
           iso3       = agglom_sel$iso3) |>
    pivot_longer(cols = all_of(names(stack)),
                 names_to = "year", values_to = "area_m2") |>
    mutate(year     = as.integer(year),
           area_km2 = area_m2 / 1e6,
           type     = label) |>
    dplyr::select(id, agglosname, iso3, year, type, area_km2,area_m2)
}

ts_total <- sum_by_city(ghsl_total, "Total")
ts_nres  <- sum_by_city(ghsl_nres,  "Non-Residential")
# 5. Combine total / non-residential, derive residential and shares ------------
# Residential = Total − Non-Residential by construction in GHSL R2023.
keys <- c("id", "agglosname", "iso3", "year")

ts_wide <- ts_total |>
  dplyr::select(all_of(keys), area_total_km2 = area_km2) |>
  left_join(ts_nres |> dplyr::select(all_of(keys), area_nres_km2 = area_km2), by = keys) |>
  mutate(
    area_res_km2 = area_total_km2 - area_nres_km2,
    share_res    = if_else(area_total_km2 > 0, area_res_km2  / area_total_km2, NA_real_),
    share_nres   = if_else(area_total_km2 > 0, area_nres_km2 / area_total_km2, NA_real_)
  )

saveRDS(ts_wide, here("data/intermediate/africapolis_builtup.Rds"))

