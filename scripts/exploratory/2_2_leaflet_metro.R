# 2_2_leaflet_metro.R
# Leaflet maps for a single African metro: residential / non-residential growth 2000–2025.
# All heavy raster ops (crop, reproject, diff) and polygon prep (validate, transform)
# are pre-computed in 1_create_citydata.R — this script is pure I/O + leaflet.
# Designed to be parameterised later (Shiny dropdown) — see `target_metro` below.
#
# Inputs:
#   data/intermediate/africapolis_builtup.Rds     (from 1_extract_ghsl.R § 5)
#   data/intermediate/cities/<slug>/
#     metro.gpkg                                  validated polygon, WGS84
#     {total,nres,res}_wgs84.tif                  WGS84 stacks
#     {res,nres}_change_2000_2025_wgs84.tif       2000→2025 diff layers
#     ntl_wgs84.tif                               annual NTL stack 2000–2024
#     ntl_change_2000_2024_wgs84.tif              NTL 2000→2024 diff
#     lit_unlit_wgs84.tif                         tri-state (0/1/2) per urban epoch
#
# Output (two separate maps to avoid layer overload):
#   output/<metro>_builtup_leaflet.html   res / nres stocks + 2000→2025 change
#   output/<metro>_ntl_leaflet.html       NTL stocks + 2000→2024 change + lit/unlit 2025

# ── 1. Setup ──────────────────────────────────────────────────────────────────
pacman::p_load(
  tidyverse, sf, here,
  leaflet, leaflet.extras, htmlwidgets, janitor,
  raster  # for raster::raster() — leaflet::addRasterImage requires it
)

dir.create(here("output"), showWarnings = FALSE)

builtup_path <- here("data/intermediate/africapolis_builtup.Rds")
if (!file.exists(builtup_path)) {
  stop("Missing ", builtup_path,
       " — run scripts/1_extract_ghsl.R through section 5 first.")
}
ts_wide <- readRDS(builtup_path)

# ── 2. Pick metro with largest absolute total built-up increase 2000→2025 ────
# To override (e.g. Shiny selection), set `target_metro` to an agglosname.
target_metro <- "Niamey"  # e.g. "Lagos", "Cairo", …

if (is.null(target_metro)) {
  target_metro <- ts_wide |>
    filter(year %in% c(2000, 2025)) |>
    dplyr::select(agglosname, year, area_total_km2) |>
    pivot_wider(names_from = year, values_from = area_total_km2,
                names_prefix = "y") |>
    mutate(delta_km2 = y2025 - y2000) |>
    slice_max(delta_km2, n = 1) |>
    pull(agglosname)
}
cat("Target metro:", target_metro, "\n")

metro_stats <- ts_wide |> filter(agglosname == target_metro)
if (nrow(metro_stats) == 0) stop("Metro '", target_metro, "' not found in ts_wide.")

# ── 3. Load pre-computed per-city assets (from 1_create_citydata.R) ──────────
city_dir <- here("data/intermediate/cities",
                 janitor::make_clean_names(target_metro))
if (!dir.exists(city_dir)) {
  stop("No per-city assets at ", city_dir,
       " — run scripts/1_create_citydata.R first.")
}

metro_sf <- st_read(file.path(city_dir, "metro.gpkg"), quiet = TRUE)

# 2000 = band 1, 2025 = band 6 in the WGS84 stacks
read_band <- function(fname, band) {
  raster::raster(file.path(city_dir, fname), band = band)
}
res_2000    <- read_band("res_wgs84.tif",  1)
res_2025    <- read_band("res_wgs84.tif",  6)
nres_2000   <- read_band("nres_wgs84.tif", 1)
nres_2025   <- read_band("nres_wgs84.tif", 6)
res_change  <- raster::raster(file.path(city_dir, "res_change_2000_2025_wgs84.tif"))
nres_change <- raster::raster(file.path(city_dir, "nres_change_2000_2025_wgs84.tif"))

# NTL: annual stack 2000–2024 (2000 = band 1, 2024 = band 25)
ntl_path     <- file.path(city_dir, "ntl_wgs84.tif")
ntl_nbands   <- raster::nbands(raster::raster(ntl_path))
ntl_2000     <- raster::raster(ntl_path, band = 1)
ntl_latest   <- raster::raster(ntl_path, band = ntl_nbands)
ntl_yr_latest <- 2000 + ntl_nbands - 1
ntl_change   <- raster::raster(file.path(city_dir,
                  sprintf("ntl_change_2000_%d_wgs84.tif", ntl_yr_latest)))

# Lit/unlit tri-state stack — band 6 = 2025 (NTL snapped from 2024)
lit_2025_r   <- raster::raster(file.path(city_dir, "lit_unlit_wgs84.tif"), band = 6)

# Unlit share for the most recent epoch
lit_vals     <- raster::values(lit_2025_r)
n_built_2025 <- sum(lit_vals >= 1, na.rm = TRUE)
n_unlit_2025 <- sum(lit_vals == 1, na.rm = TRUE)
unlit_share_2025 <- if (n_built_2025 > 0) n_unlit_2025 / n_built_2025 else NA_real_

sf_use_s2(FALSE)
metro_center <- st_centroid(st_union(metro_sf))
coords <- st_coordinates(metro_center)
metro_lng <- coords[1]
metro_lat <- coords[2]

# ── 4. Palettes ──────────────────────────────────────────────────────────────
# Stocks: 0–10 000 m²/pixel
pal_res    <- colorNumeric(c("transparent", "#e74c3c"), domain = c(0, 10000),
                           na.color = "transparent")
pal_nres   <- colorNumeric(c("transparent", "#2980b9"), domain = c(0, 10000),
                           na.color = "transparent")
# Change: -10 000 → +10 000 m²/pixel, diverging
pal_res_change  <- colorNumeric(c("#2166ac", "#f7f7f7", "#b2182b"),
                                domain = c(-10000, 10000), na.color = "transparent")
pal_nres_change <- colorNumeric(c("#2166ac", "#f7f7f7", "#b2182b"),
                                domain = c(-10000, 10000), na.color = "transparent")
# NTL: 0–30 nW/cm²/sr (clamped for visibility; brighter cores still show as max)
pal_ntl        <- colorNumeric(c("transparent", "#fff7bc", "#fec44f", "#d95f0e"),
                               domain = c(0, 50), na.color = "transparent")
pal_ntl_change <- colorNumeric(c("#2166ac", "#f7f7f7", "#b2182b"),
                               domain = c(-30, 50), na.color = "transparent")
# Lit/unlit: 0=unbuilt (transparent), 1=built-unlit (grey), 2=built-lit (yellow)
pal_lit        <- colorFactor(c("transparent", "#666666", "#f1c40f"),
                              levels = c(0, 1, 2), na.color = "transparent")

# ── 5. Leaflet maps ──────────────────────────────────────────────────────────
delta_total <- round(metro_stats$area_total_km2[metro_stats$year == 2025] -
                       metro_stats$area_total_km2[metro_stats$year == 2000], 1)

# Shared base — providers, view, metro outline
base_map <- function() {
  leaflet() |>
    addProviderTiles("CartoDB.Positron",   group = "Light") |>
    addProviderTiles("CartoDB.DarkMatter", group = "Dark") |>
    addProviderTiles("Esri.WorldImagery",  group = "Satellite") |>
    setView(lng = metro_lng, lat = metro_lat, zoom = 10) |>
    addPolygons(data = metro_sf,
                fill = FALSE, color = "#333333", weight = 1.5,
                group = "Metro boundary")
}

# 5a. Built-up map ───────────────────────────────────────────────────────────
m_builtup <- base_map() |>
  addRasterImage(res_2000,  colors = pal_res,  opacity = 0.8,
                 group = "Residential 2000") |>
  addRasterImage(nres_2000, colors = pal_nres, opacity = 0.8,
                 group = "Non-Residential 2000") |>
  addRasterImage(res_2025,  colors = pal_res,  opacity = 0.8,
                 group = "Residential 2025") |>
  addRasterImage(nres_2025, colors = pal_nres, opacity = 0.8,
                 group = "Non-Residential 2025") |>
  addRasterImage(res_change,  colors = pal_res_change,  opacity = 0.85,
                 group = "Residential growth 2000→2025") |>
  addRasterImage(nres_change, colors = pal_nres_change, opacity = 0.85,
                 group = "Non-Residential growth 2000→2025") |>
  addLayersControl(
    baseGroups    = c("Light", "Dark", "Satellite"),
    overlayGroups = c("Metro boundary",
                      "Residential 2000",  "Residential 2025",
                      "Non-Residential 2000", "Non-Residential 2025",
                      "Residential growth 2000→2025",
                      "Non-Residential growth 2000→2025"),
    options       = layersControlOptions(collapsed = FALSE)
  ) |>
  addLegend(pal = pal_res,         values = c(0, 10000),
            title = "Residential<br>(m²/pixel)",     position = "bottomleft") |>
  addLegend(pal = pal_nres,        values = c(0, 10000),
            title = "Non-Residential<br>(m²/pixel)", position = "bottomleft") |>
  addLegend(pal = pal_res_change,  values = c(-10000, 10000),
            title = "Δ Residential<br>2000–2025",    position = "bottomright") |>
  addLegend(pal = pal_nres_change, values = c(-10000, 10000),
            title = "Δ Non-Residential<br>2000–2025", position = "bottomright") |>
  addControl(
    html = sprintf(
      "<b>%s</b><br>Total built-up Δ 2000–2025: <b>%+.1f km²</b>",
      target_metro, delta_total),
    position = "topright"
  ) |>
  hideGroup(c("Residential 2000", "Non-Residential 2000",
              "Residential growth 2000→2025",
              "Non-Residential growth 2000→2025"))

# 5b. Nighttime-lights map ───────────────────────────────────────────────────
ntl_grp_2000   <- "Nighttime lights 2000"
ntl_grp_latest <- sprintf("Nighttime lights %d", ntl_yr_latest)
ntl_grp_change <- sprintf("NTL growth 2000→%d", ntl_yr_latest)
ntl_grp_lit    <- "Lit / unlit settlements 2025"

m_ntl <- base_map() |>
  addRasterImage(ntl_2000,   colors = pal_ntl,        opacity = 0.85,
                 group = ntl_grp_2000) |>
  addRasterImage(ntl_latest, colors = pal_ntl,        opacity = 0.85,
                 group = ntl_grp_latest) |>
  addRasterImage(ntl_change, colors = pal_ntl_change, opacity = 0.85,
                 group = ntl_grp_change) |>
  addRasterImage(lit_2025_r, colors = pal_lit, opacity = 0.85, method = "ngb",
                 group = ntl_grp_lit) |>
  addLayersControl(
    baseGroups    = c("Light", "Dark", "Satellite"),
    overlayGroups = c("Metro boundary",
                      ntl_grp_2000, ntl_grp_latest,
                      ntl_grp_change, ntl_grp_lit),
    options       = layersControlOptions(collapsed = FALSE)
  ) |>
  addLegend(pal = pal_ntl,        values = c(0, 50),
            title = "NTL<br>(nW/cm²/sr)",   position = "bottomleft") |>
  addLegend(pal = pal_ntl_change, values = c(-30, 50),
            title = sprintf("Δ NTL<br>2000–%d", ntl_yr_latest),
            position = "bottomright") |>
  addLegend(colors = c("#666666", "#f1c40f"),
            labels = c("Built · unlit", "Built · lit"),
            title  = "Settlements 2025", position = "bottomleft") |>
  addControl(
    html = sprintf(
      "<b>%s</b><br>Unlit share of built-up 2025: <b>%s</b>",
      target_metro,
      if (is.na(unlit_share_2025)) "n/a"
      else sprintf("%.1f%%", 100 * unlit_share_2025)),
    position = "topright"
  ) |>
  hideGroup(c(ntl_grp_2000, ntl_grp_change, ntl_grp_lit))
  # default visible: NTL latest year only

# ── 6. Save both maps ────────────────────────────────────────────────────────
slug             <- janitor::make_clean_names(target_metro)
builtup_out_path <- here("output", paste0(slug, "_builtup_leaflet.html"))
ntl_out_path     <- here("output", paste0(slug, "_ntl_leaflet.html"))

htmlwidgets::saveWidget(m_builtup, builtup_out_path, selfcontained = TRUE)
htmlwidgets::saveWidget(m_ntl,     ntl_out_path,     selfcontained = TRUE)

cat("\nDone. Maps written to:\n  ", builtup_out_path, "\n  ", ntl_out_path, "\n")
