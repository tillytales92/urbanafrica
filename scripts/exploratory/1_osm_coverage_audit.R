# 1_osm_coverage_audit.R  (exploratory)
# --------------------------------------------------------------------------
# Before spending effort generalizing 1_osm_load.R to all 100 cities: how
# uneven is OSM's building/road coverage across the top-100 African
# agglomerations? Coverage tracks mapping effort (HOT/humanitarian campaigns),
# not urban form, so any cross-city OSM metric is only meaningful for cities
# that clear a coverage floor. This is a lightweight scoping check, not the
# full per-city geometry pull -- Overpass `out count;` returns element counts
# without downloading any geometry, so it's cheap enough to run on all 100
# bounding boxes in one pass.
#
# Consumes:
#   data/intermediate/agglom_top100.gpkg
# Produces:
#   data/intermediate/osm_coverage_audit.Rds
# --------------------------------------------------------------------------

pacman::p_load(sf, here, dplyr, httr, jsonlite)

sf::sf_use_s2(FALSE)

agglom_100 <- st_read(here("data/intermediate/agglom_top100.gpkg"), quiet = TRUE)

# Polygon area (Mollweide, equal-area) -- used to normalise counts per km2.
# Deliberately NOT the bbox area: bbox overcounts area (and thus undercounts
# density) inconsistently depending on how elongated/irregular each polygon
# is, which would bias the density comparison across cities.
agglom_100 <- agglom_100 |>
  mutate(area_km2 = as.numeric(st_area(st_transform(st_geometry(agglom_100), "ESRI:54009"))) / 1e6)

overpass_endpoints <- c(
  "https://overpass-api.de/api/interpreter",
  "https://overpass.kumi.systems/api/interpreter"
)

# Single query per city: building-way and highway-way counts over its bbox,
# via Overpass `out count;` (counts only, no geometry download).
count_osm <- function(bbox_str, tries_per_endpoint = 2) {
  q <- sprintf('
[out:json][timeout:120];
way["building"](%s)->.b;
way["highway"](%s)->.h;
.b out count;
.h out count;
', bbox_str, bbox_str)

  for (ep in overpass_endpoints) {
    for (i in seq_len(tries_per_endpoint)) {
      resp <- tryCatch(
        httr::POST(ep, body = q, encode = "raw", httr::timeout(150)),
        error = function(e) NULL
      )
      if (!is.null(resp) && httr::status_code(resp) == 200) {
        js <- tryCatch(
          jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                              simplifyVector = FALSE),
          error = function(e) NULL
        )
        if (!is.null(js) && length(js$elements) == 2) {
          return(list(
            buildings = as.integer(js$elements[[1]]$tags$total),
            highways  = as.integer(js$elements[[2]]$tags$total)
          ))
        }
      }
      Sys.sleep(3)
    }
  }
  list(buildings = NA_integer_, highways = NA_integer_)
}

out_file <- here("data/intermediate/osm_coverage_audit.Rds")
results  <- vector("list", nrow(agglom_100))

for (i in seq_len(nrow(agglom_100))) {
  city <- agglom_100[i, ]
  bb   <- st_bbox(city)
  bbox_str <- sprintf("%.5f,%.5f,%.5f,%.5f", bb["ymin"], bb["xmin"], bb["ymax"], bb["xmax"])

  r <- count_osm(bbox_str)
  results[[i]] <- tibble(
    slug        = city$slug,
    agglos_name = city$agglos_name,
    iso3        = city$iso3,
    area_km2    = city$area_km2,
    buildings   = r$buildings,
    highways    = r$highways
  )
  message(sprintf("[%3d/100] %-30s buildings=%s highways=%s (%.0f km2)",
                   i, city$agglos_name,
                   ifelse(is.na(r$buildings), "NA", format(r$buildings, big.mark = ",")),
                   ifelse(is.na(r$highways),  "NA", format(r$highways,  big.mark = ",")),
                   city$area_km2))

  # Write incrementally so a mid-run failure still leaves usable partial data.
  saveRDS(bind_rows(results), out_file)
  Sys.sleep(1.5)
}

audit <- bind_rows(results) |>
  mutate(
    buildings_per_km2 = buildings / area_km2,
    highways_per_km2  = highways  / area_km2
  )
saveRDS(audit, out_file)

cat("\n==== Coverage summary ====\n")
cat(sprintf("Scored: %d / %d cities\n", sum(!is.na(audit$buildings)), nrow(audit)))
print(summary(audit$buildings_per_km2))
cat("\n-- Best covered (buildings/km2) --\n")
print(audit |> arrange(desc(buildings_per_km2)) |>
        select(agglos_name, iso3, area_km2, buildings, buildings_per_km2) |> head(10))
cat("\n-- Worst covered (buildings/km2) --\n")
print(audit |> arrange(buildings_per_km2) |>
        select(agglos_name, iso3, area_km2, buildings, buildings_per_km2) |> head(10))
