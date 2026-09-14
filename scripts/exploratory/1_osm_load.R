# 1_osm_load.R  (exploratory / prototype)
# --------------------------------------------------------------------------
# Infrastructure to pull OpenStreetMap layers for the Africapolis
# agglomerations we track, then a single-city test run (default: Accra) to
# see what OSM actually gives us for an African metro.
#
# Approach: Overpass API via {osmdata}. Query by the agglomeration bounding
# box, then clip every layer to the agglomeration polygon. One folder per
# city under data/intermediate/osm/<slug>/, one .gpkg per layer, so the rest
# of the pipeline can read small local files.
#
# Overpass is fine for one city at a time. Scaling to all 100 later probably
# wants {osmextract} + Geofabrik country .pbf extracts instead (bulk, offline,
# no rate limits) -- swap fetch_osm_layer() for an oe_read() path then.
#
# Consumes:
#   data/raw/africapolis/agglomerations.shp
# Produces:
#   data/intermediate/osm/<slug>/{roads,buildings,landuse,water}.gpkg
#   output/exploratory/osm_<slug>.png
# --------------------------------------------------------------------------

pacman::p_load(tidyverse, sf, here, janitor, osmdata, scales)

sf::sf_use_s2(FALSE)

# -- config ----------------------------------------------------------------
target_slug <- "accra"           # change to run another city
osm_root    <- here("data/intermediate/osm")
dir.create(osm_root, showWarnings = FALSE, recursive = TRUE)
dir.create(here("output/exploratory"), showWarnings = FALSE, recursive = TRUE)

# -- 1. agglomeration polygon (canonical set from 0_simplifyshapefile.R) ------
gpkg <- here("data/intermediate/agglom_top100.gpkg")
if (!file.exists(gpkg)) stop("Missing ", gpkg, " -- run scripts/0_simplifyshapefile.R first.")
agglom_100 <- st_read(gpkg, quiet = TRUE)

city <- agglom_100 |> filter(slug == target_slug)
if (nrow(city) != 1) {
  stop("Expected exactly one agglomeration for slug '", target_slug,
       "' -- found ", nrow(city), ".\nAvailable slugs:\n  ",
       paste(sort(agglom_100$slug), collapse = "\n  "))
}

city   <- st_transform(city, 4326)
poly   <- st_geometry(city)
bb     <- st_bbox(city)
bbox_v <- c(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"]) |> unname()

message(sprintf("City: %s (%s) — bbox %.3f,%.3f,%.3f,%.3f",
                city$agglos_name, city$iso3, bbox_v[1], bbox_v[2], bbox_v[3], bbox_v[4]))

# -- 2. Overpass fetch helper ---------------------------------------------------
# One key (optionally value-filtered), returned as the requested geometry
# type, clipped to the agglomeration polygon. Retries once on transient
# Overpass failure.
fetch_osm_layer <- function(bbox, key, value = NULL,
                            geom = c("polygons", "lines", "points"),
                            clip_to = poly, tries = 2) {
  geom <- match.arg(geom)
  q <- opq(bbox = bbox, timeout = 180)
  q <- add_osm_feature(q, key = key, value = value)

  dat <- NULL
  for (i in seq_len(tries)) {
    dat <- tryCatch(osmdata_sf(q), error = function(e) {
      message(sprintf("  Overpass attempt %d/%d failed for %s: %s",
                      i, tries, key, conditionMessage(e)))
      NULL
    })
    if (!is.null(dat)) break
    Sys.sleep(5)
  }
  if (is.null(dat)) return(NULL)

  g <- switch(geom,
              polygons = dat$osm_polygons,
              lines    = dat$osm_lines,
              points   = dat$osm_points)
  if (is.null(g) || nrow(g) == 0) return(NULL)

  g <- st_make_valid(g)
  suppressWarnings(st_intersection(g, clip_to))
}

# -- 3. pull the layers -------------------------------------------------------
message("Fetching roads (highway) ...")
roads <- fetch_osm_layer(bbox_v, "highway", geom = "lines")

message("Fetching buildings ...")
buildings <- fetch_osm_layer(bbox_v, "building", geom = "polygons")

message("Fetching landuse ...")
landuse <- fetch_osm_layer(bbox_v, "landuse", geom = "polygons")

message("Fetching water (natural=water) ...")
water <- fetch_osm_layer(bbox_v, "natural", value = "water", geom = "polygons")

# -- 4. save ----------------------------------------------------------------
out_dir <- file.path(osm_root, target_slug)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

save_layer <- function(x, name) {
  if (is.null(x) || nrow(x) == 0) {
    message("  (no ", name, " features)")
    return(invisible())
  }
  # gpkg dislikes list-columns and mixed geometry; keep it simple
  x <- x |>
    select(where(~ !is.list(.x)) | any_of("geometry")) |>
    st_cast(if (name == "roads") "MULTILINESTRING" else "MULTIPOLYGON", warn = FALSE)
  st_write(x, file.path(out_dir, paste0(name, ".gpkg")),
           delete_dsn = TRUE, quiet = TRUE)
  message(sprintf("  wrote %s.gpkg (%d features)", name, nrow(x)))
}

save_layer(roads,     "roads")
save_layer(buildings, "buildings")
save_layer(landuse,   "landuse")
save_layer(water,     "water")

# -- 5. what did we get? ----------------------------------------------------
# metric CRS for length / area (Mollweide, project-wide equal-area choice)
to_m <- function(x) if (is.null(x)) NULL else st_transform(x, "ESRI:54009")
poly_m    <- st_transform(poly, "ESRI:54009")
area_km2  <- as.numeric(st_area(poly_m)) / 1e6

roads_m <- to_m(roads); build_m <- to_m(buildings); lu_m <- to_m(landuse)

cat("\n==== OSM coverage — ", city$agglos_name, " (", city$iso3, ") ====\n", sep = "")
cat(sprintf("Agglomeration area: %.0f km2\n", area_km2))

if (!is.null(roads_m)) {
  roads_m$len_km <- as.numeric(st_length(roads_m)) / 1e3
  by_class <- roads_m |>
    st_drop_geometry() |>
    mutate(highway = as.character(highway)) |>
    group_by(highway) |>
    summarise(n = n(), km = sum(len_km), .groups = "drop") |>
    arrange(desc(km))
  cat(sprintf("\nRoads: %d segments, %.0f km total (%.2f km / km2)\n",
              nrow(roads_m), sum(roads_m$len_km), sum(roads_m$len_km) / area_km2))
  print(as.data.frame(by_class), row.names = FALSE, digits = 4)
}

if (!is.null(build_m)) {
  build_m$fp_m2 <- as.numeric(st_area(build_m))
  lv <- suppressWarnings(as.numeric(build_m[["building:levels"]]))
  cat(sprintf("\nBuildings: %d footprints, %.2f km2 total footprint (%.1f%% of agglomeration)\n",
              nrow(build_m), sum(build_m$fp_m2) / 1e6,
              100 * sum(build_m$fp_m2) / (area_km2 * 1e6)))
  cat(sprintf("  with building:levels tag: %d (%.1f%%)\n",
              sum(!is.na(lv)), 100 * mean(!is.na(lv))))
}

if (!is.null(lu_m)) {
  lu_m$a_km2 <- as.numeric(st_area(lu_m)) / 1e6
  lu_tab <- lu_m |>
    st_drop_geometry() |>
    mutate(landuse = as.character(landuse)) |>
    group_by(landuse) |>
    summarise(n = n(), km2 = sum(a_km2), .groups = "drop") |>
    arrange(desc(km2))
  cat(sprintf("\nLanduse polygons: %d, %.1f km2 classified\n", nrow(lu_m), sum(lu_m$a_km2)))
  print(as.data.frame(lu_tab), row.names = FALSE, digits = 4)
}

# -- 6. quick look PNG ----------------------------------------------------------
p <- ggplot() +
  { if (!is.null(landuse))   geom_sf(data = landuse,  fill = "#e8e2d0", colour = NA) } +
  { if (!is.null(water))     geom_sf(data = water,    fill = "#a9cce3", colour = NA) } +
  { if (!is.null(buildings)) geom_sf(data = buildings, fill = "#c0392b", colour = NA, linewidth = 0) } +
  { if (!is.null(roads))     geom_sf(data = roads,    colour = "#34495e", linewidth = 0.2) } +
  geom_sf(data = poly, fill = NA, colour = "black", linewidth = 0.6) +
  labs(title = paste0("OSM — ", city$agglos_name, " (", city$iso3, ")"),
       subtitle = "buildings (red) · roads (grey) · landuse (tan) · water (blue)") +
  theme_void(base_size = 12)

ggsave(here("output/exploratory", paste0("osm_", target_slug, ".png")), p,
       width = 10, height = 10, dpi = 300)

cat("\nWrote", file.path("data/intermediate/osm", target_slug),
    "and output/exploratory/osm_", target_slug, ".png\n", sep = "")
