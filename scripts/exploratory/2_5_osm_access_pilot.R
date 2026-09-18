# 2_5_osm_access_pilot.R  (exploratory / pilot)
# --------------------------------------------------------------------------
# Pilot: within a city, did built-up densification (population growth inside
# the existing 1990 footprint) put more people within reach of OSM-tagged
# public infrastructure than new extensive-margin growth (sprawl onto
# previously-empty land)?
#
# Deliberately a per-city case study, not a cross-city metric: the OSM
# coverage audit (1_osm_coverage_audit.R) found building/road density spans
# a >900x range across the top-100 and tracks mapping effort (HOT/
# humanitarian campaigns), not urban form. Comparing *distances within one
# city* sidesteps that -- mapping effort is roughly constant within a city,
# even if absolute amenity counts aren't comparable across cities. Piloted
# on three of the audit's best-covered agglomerations (>1,300 buildings/km2):
# Lagos (West), Kinshasa (Central -- the most extreme densifier in the
# 1990-2025 sample, see docs/insights-1990-2025.md SS9), Kampala (East).
#
# Method:
#   1. Classify every 100m pixel by 1990->2025 built-up trajectory (mirrors
#      growth_stats() in app/app.R): sprawl (new footprint), intensification
#      (built in 1990, densified by 2025), static/unbuilt.
#   2. Pull OSM schools + healthcare + drinking-water points (Overpass).
#   3. Distance-to-nearest-amenity raster (terra::distance), per category and
#      combined.
#   4. Population-weighted (GHS-POP 2025) mean distance by growth category --
#      "did the people added by densification end up closer to, or farther
#      from, public infrastructure than people added by sprawl?"
#
# Consumes:
#   data/intermediate/cities/<slug>/{pop.tif, total.tif, metro.gpkg}
# Produces:
#   data/intermediate/osm_access_pilot.Rds
#   output/exploratory/osm_access_<slug>.png
# --------------------------------------------------------------------------

pacman::p_load(tidyverse, terra, sf, here, httr, jsonlite, scales)

sf::sf_use_s2(FALSE)

pilot_slugs <- c("lagos", "kinshasa", "kampala")
cities_root <- here("data/intermediate/cities")
out_dir     <- here("output/exploratory")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -- Overpass fetch helper -- raw httr::POST + Overpass QL, exactly the
# pattern 1_osm_coverage_audit.R already uses reliably. NOT osmdata::
# osmdata_sf()'s internal httr2 retry layer hangs indefinitely on both
# overpass-api.de and overpass.kumi.systems in this environment (confirmed
# in this session -- raw POST returns in ~1.5s against the same endpoint
# osmdata_sf() never got past). Nodes only (not ways) -- point amenities are
# overwhelmingly mapped as nodes, and a pilot doesn't need way-centroid
# complexity.
overpass_endpoints <- c(
  "https://overpass-api.de/api/interpreter",
  "https://overpass.kumi.systems/api/interpreter"
)

fetch_osm_points <- function(ql_selectors, tries_per_endpoint = 2) {
  q <- sprintf('[out:json][timeout:90];\n(\n%s\n);\nout body;',
               paste(ql_selectors, collapse = "\n"))
  for (ep in overpass_endpoints) {
    for (i in seq_len(tries_per_endpoint)) {
      resp <- tryCatch(
        httr::POST(ep, body = q, encode = "raw", httr::timeout(100)),
        error = function(e) NULL
      )
      if (!is.null(resp) && httr::status_code(resp) == 200) {
        js <- tryCatch(
          jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                              simplifyVector = FALSE),
          error = function(e) NULL
        )
        if (!is.null(js) && length(js$elements) > 0) {
          lon <- purrr::map_dbl(js$elements, "lon")
          lat <- purrr::map_dbl(js$elements, "lat")
          return(sf::st_as_sf(tibble(lon = lon, lat = lat),
                               coords = c("lon", "lat"), crs = 4326))
        }
        return(NULL)  # 200 but zero elements -- genuinely no matches, don't retry
      }
      Sys.sleep(3)
    }
  }
  NULL
}

# amenity=school | healthcare (hospital/clinic/doctors) | water (drinking
# water points + wells) -- the three "local public goods" Delbridge et al.
# (2022) discuss as access-critical. One Overpass QL selector set per
# category (bbox substituted in per-city).
amenity_ql <- list(
  school     = 'node["amenity"="school"](%s);',
  healthcare = 'node["amenity"~"^(hospital|clinic|doctors)$"](%s);',
  water      = 'node["amenity"="drinking_water"](%s);\nnode["man_made"="water_well"](%s);'
)

# -- per-city pipeline -------------------------------------------------------
run_city <- function(slug) {
  message("\n==== ", slug, " ====")
  d <- file.path(cities_root, slug)

  pop <- terra::rast(file.path(d, "pop.tif"))
  tot <- terra::rast(file.path(d, "total.tif"))
  metro <- sf::st_read(file.path(d, "metro.gpkg"), quiet = TRUE)
  bb <- sf::st_bbox(metro)
  # Overpass bbox order is south,west,north,east (lat,lon,lat,lon).
  bbox_str <- sprintf("%.5f,%.5f,%.5f,%.5f", bb["ymin"], bb["xmin"], bb["ymax"], bb["xmax"])

  pop_1990 <- pop[["1990"]]; pop_2025 <- pop[["2025"]]
  tot_1990 <- tot[["1990"]]; tot_2025 <- tot[["2025"]]

  # Growth category per pixel -- mirrors growth_stats() in app/app.R.
  # terra::ifel() only returns SpatRaster/numeric/logical, not character, so
  # code as integers (1 sprawl, 2 intensification, 3 static/declined, NA
  # never-built) and label after extracting to a data frame.
  cat_labels <- c(`1` = "Sprawl (new footprint)",
                  `2` = "Intensification (densified)",
                  `3` = "Static/declined")
  category <- terra::ifel(
    tot_1990 == 0 & tot_2025 > 0, 1,
    terra::ifel(tot_1990 > 0 & tot_2025 > tot_1990, 2,
                terra::ifel(tot_1990 > 0, 3, NA))
  )
  pop_change <- pop_2025 - pop_1990

  # -- OSM amenities -----------------------------------------------------
  cats <- purrr::imap(amenity_ql, function(ql_tpl, name) {
    message("  fetching ", name, " ...")
    n_ph <- lengths(regmatches(ql_tpl, gregexpr("%s", ql_tpl)))
    selector <- do.call(sprintf, c(list(ql_tpl), as.list(rep(bbox_str, n_ph))))
    pts <- fetch_osm_points(selector)
    if (!is.null(pts)) message("    ", nrow(pts), " points")
    pts
  })
  cats <- purrr::compact(cats)
  if (length(cats) == 0) {
    message("  No OSM amenities returned for ", slug, " -- skipping.")
    return(NULL)
  }
  all_pts <- bind_rows(cats)

  # Distance-to-nearest raster per category + combined, in the native
  # Mollweide grid (metres -> km). terra::distance() gives distance to the
  # nearest non-NA cell.
  template <- tot_1990; template[] <- NA
  dist_for <- function(pts_sf) {
    if (is.null(pts_sf) || nrow(pts_sf) == 0) return(NULL)
    pts_v <- terra::vect(sf::st_transform(pts_sf, terra::crs(tot)))
    r <- terra::rasterize(pts_v, template, field = 1)
    terra::distance(r) / 1000
  }
  dist_combined <- dist_for(all_pts)

  if (is.null(dist_combined)) {
    message("  Distance raster failed for ", slug, " -- skipping.")
    return(NULL)
  }

  # -- Pop-weighted summary by growth category ----------------------------
  df <- tibble(
    growth_code = terra::values(category)[, 1],
    pop_2025    = terra::values(pop_2025)[, 1],
    pop_change  = terra::values(pop_change)[, 1],
    dist_km     = terra::values(dist_combined)[, 1]
  ) |>
    filter(!is.na(growth_code), !is.na(dist_km)) |>
    mutate(growth_cat = unname(cat_labels[as.character(growth_code)]))

  summary_tbl <- df |>
    group_by(growth_cat) |>
    summarise(
      n_pixels          = n(),
      pop_2025_total    = sum(pop_2025, na.rm = TRUE),
      pop_added_total   = sum(pmax(pop_change, 0), na.rm = TRUE),
      mean_dist_km      = mean(dist_km, na.rm = TRUE),
      pop_weighted_dist = weighted.mean(dist_km, w = pmax(pop_2025, 0.001), na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(slug = slug, .before = 1)

  print(as.data.frame(summary_tbl), row.names = FALSE, digits = 3)

  # -- quick-look map -------------------------------------------------------
  cat_df <- as.data.frame(category, xy = TRUE) |>
    rename(growth_code = 3) |>
    filter(!is.na(growth_code)) |>
    mutate(growth_cat = unname(cat_labels[as.character(growth_code)]))
  p <- ggplot() +
    geom_raster(data = cat_df, aes(x, y, fill = growth_cat)) +
    geom_sf(data = sf::st_transform(all_pts, terra::crs(tot)), size = 0.3, colour = "black", alpha = 0.5) +
    scale_fill_manual(values = c("Sprawl (new footprint)" = "#e67e22",
                                 "Intensification (densified)" = "#2980b9",
                                 "Static/declined" = "#bdc3c7"),
                       name = "1990→2025") +
    coord_sf(datum = terra::crs(tot)) +
    labs(title = paste0("OSM access pilot — ", slug),
         subtitle = "points = OSM schools/healthcare/water; colour = built-up growth category") +
    theme_void(base_size = 11)
  ggsave(file.path(out_dir, paste0("osm_access_", slug, ".png")), p,
         width = 9, height = 9, dpi = 250)

  list(summary = summary_tbl, n_amenities = nrow(all_pts))
}

results <- purrr::map(pilot_slugs, run_city)
names(results) <- pilot_slugs
results <- purrr::compact(results)

summary_all <- purrr::map_dfr(results, "summary")
saveRDS(summary_all, here("data/intermediate/osm_access_pilot.Rds"))

cat("\n==== Combined summary (all pilot cities) ====\n")
print(as.data.frame(summary_all), row.names = FALSE, digits = 3)

cat("\nWrote data/intermediate/osm_access_pilot.Rds and",
    length(results), "output/exploratory/osm_access_<slug>.png map(s)\n")
