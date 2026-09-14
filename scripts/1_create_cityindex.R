# Build a small index of per-city metadata for the Shiny app.
# Produces data/intermediate/city_index.Rds — read once at app start so the
# app can build the city dropdown, fly the map to a centroid, and show the
# total-Δ summary without scanning the cities/ directory, opening metro.gpkg,
# or pulling raster values at runtime.

pacman::p_load(tidyverse, terra, sf, here, janitor)

sf::sf_use_s2(FALSE)

cities_root <- here("data/intermediate/cities")
builtup     <- readRDS(here("data/intermediate/africapolis_builtup.Rds"))

slugs <- list.dirs(cities_root, recursive = FALSE, full.names = FALSE)

# Built-up summary: total Δ 1990→2025 per city, indexed by slug.
# 1990 is the analytical headline start (GHS-BUILT-S 8-epoch set). NTL / lit-unlit
# stays referenced at 2000 (corrected DMSP starts 1992; pre-2000 is noisier).
# Slug must be computed on unique names — make_clean_names() dedupes by
# appending "_2"/"_3" when given a vector with repeats (one row per year).
slug_map <- builtup |>
  distinct(agglosname) |>
  mutate(slug = janitor::make_clean_names(agglosname))

builtup_summary <- builtup |>
  left_join(slug_map, by = "agglosname") |>
  filter(slug %in% slugs, year %in% c(1990, 2025)) |>
  select(slug, agglosname, iso3, year, area_total_km2) |>
  pivot_wider(names_from = year, values_from = area_total_km2,
              names_prefix = "total_km2_") |>
  mutate(delta_total_km2_1990_2025 = total_km2_2025 - total_km2_1990)

# Per-slug spatial + lit-share metadata.
build_row <- function(slug) {
  dir   <- file.path(cities_root, slug)
  metro <- sf::st_read(file.path(dir, "metro.gpkg"), quiet = TRUE)

  ctr   <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_union(metro))))
  bb    <- sf::st_bbox(metro)

  # lit_unlit_wgs84.tif is 4-state: 0 unbuilt, 1 built-unlit (DN=0),
  # 2 built-dim (0<DN<=10), 3 built-bright (DN>10). Shares are over built-up.
  # Bands are the urban epochs, named by year; select by name, not position.
  # We carry 2000 and 2025 so the app can show a 2000->2025 change row.
  lit_f <- file.path(dir, "lit_unlit_wgs84.tif")
  na_sh <- c(unlit = NA_real_, dim = NA_real_, darkdim = NA_real_)
  lu_shares <- function(band) {
    v  <- terra::values(band, na.rm = TRUE)
    nb <- sum(v >= 1)
    if (nb == 0) return(na_sh)
    c(unlit   = sum(v == 1)        / nb,
      dim     = sum(v == 2)        / nb,
      darkdim = sum(v %in% c(1, 2)) / nb)
  }
  lu_year <- function(r, yr) {
    if (as.character(yr) %in% names(r)) lu_shares(r[[as.character(yr)]]) else na_sh
  }
  s2000 <- s2025 <- na_sh
  if (file.exists(lit_f)) {
    r <- terra::rast(lit_f)
    s2000 <- lu_year(r, 2000)
    s2025 <- lu_year(r, 2025)
  }

  ntl_f <- file.path(dir, "ntl_wgs84.tif")
  ntl_yrs <- if (file.exists(ntl_f)) as.integer(names(terra::rast(ntl_f))) else integer(0)

  tibble(
    slug               = slug,
    lon                = unname(ctr[1, "X"]),
    lat                = unname(ctr[1, "Y"]),
    xmin               = unname(bb["xmin"]),
    ymin               = unname(bb["ymin"]),
    xmax               = unname(bb["xmax"]),
    ymax               = unname(bb["ymax"]),
    unlit_share_2000   = unname(s2000["unlit"]),
    dim_share_2000     = unname(s2000["dim"]),
    darkdim_share_2000 = unname(s2000["darkdim"]),
    unlit_share_2025   = unname(s2025["unlit"]),
    dim_share_2025     = unname(s2025["dim"]),
    darkdim_share_2025 = unname(s2025["darkdim"]),
    ntl_n_bands        = length(ntl_yrs),
    ntl_year_latest    = if (length(ntl_yrs) > 0) max(ntl_yrs) else NA_integer_
  )
}

spatial_meta <- purrr::map_dfr(slugs, build_row, .progress = TRUE)

city_index <- builtup_summary |>
  inner_join(spatial_meta, by = "slug") |>
  arrange(desc(total_km2_2025))

saveRDS(city_index, here("data/intermediate/city_index.Rds"))

message(sprintf("Wrote city_index.Rds with %d cities.", nrow(city_index)))
