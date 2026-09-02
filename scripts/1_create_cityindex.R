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

# Built-up summary: total Δ 2000→2025 per city, indexed by slug.
# Slug must be computed on unique names — make_clean_names() dedupes by
# appending "_2"/"_3" when given a vector with repeats (one row per year).
slug_map <- builtup |>
  distinct(agglosname) |>
  mutate(slug = janitor::make_clean_names(agglosname))

builtup_summary <- builtup |>
  left_join(slug_map, by = "agglosname") |>
  filter(slug %in% slugs, year %in% c(2000, 2025)) |>
  select(slug, agglosname, iso3, year, area_total_km2) |>
  pivot_wider(names_from = year, values_from = area_total_km2,
              names_prefix = "total_km2_") |>
  mutate(delta_total_km2_2000_2025 = total_km2_2025 - total_km2_2000)

# Per-slug spatial + lit-share metadata.
build_row <- function(slug) {
  dir   <- file.path(cities_root, slug)
  metro <- sf::st_read(file.path(dir, "metro.gpkg"), quiet = TRUE)

  ctr   <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_union(metro))))
  bb    <- sf::st_bbox(metro)

  lit_f <- file.path(dir, "lit_unlit_wgs84.tif")
  unlit_share_2025 <- NA_real_
  if (file.exists(lit_f)) {
    r <- terra::rast(lit_f)
    if (terra::nlyr(r) >= 6) {
      v  <- terra::values(r[[6]], na.rm = TRUE)
      nb <- sum(v >= 1)
      nu <- sum(v == 1)
      unlit_share_2025 <- if (nb > 0) nu / nb else NA_real_
    }
  }

  ntl_f <- file.path(dir, "ntl_wgs84.tif")
  ntl_n_bands <- if (file.exists(ntl_f)) terra::nlyr(terra::rast(ntl_f)) else 0L

  tibble(
    slug             = slug,
    lon              = unname(ctr[1, "X"]),
    lat              = unname(ctr[1, "Y"]),
    xmin             = unname(bb["xmin"]),
    ymin             = unname(bb["ymin"]),
    xmax             = unname(bb["xmax"]),
    ymax             = unname(bb["ymax"]),
    unlit_share_2025 = unlit_share_2025,
    ntl_n_bands      = ntl_n_bands,
    ntl_year_latest  = if (ntl_n_bands > 0) 2000L + ntl_n_bands - 1L else NA_integer_
  )
}

spatial_meta <- purrr::map_dfr(slugs, build_row, .progress = TRUE)

city_index <- builtup_summary |>
  inner_join(spatial_meta, by = "slug") |>
  arrange(desc(total_km2_2025))

saveRDS(city_index, here("data/intermediate/city_index.Rds"))

message(sprintf("Wrote city_index.Rds with %d cities.", nrow(city_index)))
