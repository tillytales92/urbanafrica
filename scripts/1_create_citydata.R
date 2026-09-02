# Per-city raster splitting (for Shiny / per-metro leaflet) -----------------
# Pre-cuts the continental stacks into one folder per agglomeration and
# pre-computes everything the leaflet/Shiny layer needs at runtime:
#   data/intermediate/cities/<slug>/
#     metro.gpkg                              validated polygon, WGS84
#     {total,nres,res}.tif                    native-CRS stacks
#     {total,nres,res}_wgs84.tif              WGS84 stacks (for leaflet)
#     {res,nres}_change_2000_2025_wgs84.tif   2000→2025 diff (for leaflet)
#     ntl.tif / ntl_wgs84.tif                 nighttime-lights stack
#     ntl_change_<y0>_<y1>_wgs84.tif          first→last year NTL diff
#     lit_unlit.tif / lit_unlit_wgs84.tif     tri-state: 0=unbuilt, 1=built-unlit, 2=built-lit
#                                             (NTL snapped to urban epochs; 2025 uses 2024)
# Downstream scripts read these small files directly — no reprojection,
# no st_make_valid, no raster algebra at map-build time.

#### 1. Setup ####
pacman::p_load(
  tidyverse, terra, sf, here,
  ggplot2, scales,janitor)

#set target folder
cities_root <- here("data/intermediate/cities")
dir.create(cities_root, showWarnings = FALSE, recursive = TRUE)

#Load cropped raster stacks
#TOTAL
total_africa <- terra::rast(here("data/intermediate/raster/total_africa.tif"))
#NRES
nres_africa <- terra::rast(here("data/intermediate/raster/nres_africa.tif"))
#RES
res_africa <- terra::rast(here("data/intermediate/raster/res_africa.tif"))
#NTL
ntl_africa <- terra::rast(here("data/intermediate/raster/ntl_urbanafrica.tif"))

#Load Agglom Shapefile
# Urban areas in Africa
agglom <- st_read(here("data/raw/africapolis/agglomerations.shp")) |>
  clean_names()

#Urban areas in Africa: largest 100
agglom_100 <- agglom |>
  slice_max(pop2020,n = 100) |>
  st_make_valid()

#Processing function
process_city <- function(city_row, total_r, nres_r, ntl_r,
                         out_root = cities_root, overwrite = FALSE,
                         ntl_threshold = 0.5) {
  slug    <- janitor::make_clean_names(city_row$agglos_name)
  out_dir <- file.path(out_root, slug)

  poly_file    <- file.path(out_dir, "metro.gpkg")
  native_files <- file.path(out_dir, c("total.tif", "nres.tif", "res.tif"))
  wgs_files    <- file.path(out_dir, c("total_wgs84.tif", "nres_wgs84.tif", "res_wgs84.tif"))
  change_files <- file.path(out_dir, c("res_change_2000_2025_wgs84.tif",
                                       "nres_change_2000_2025_wgs84.tif"))
  ext_files    <- file.path(out_dir, c("res_ext_2000_2025_wgs84.tif",
                                       "nres_ext_2000_2025_wgs84.tif"))
  urban_files  <- c(poly_file, native_files, wgs_files, change_files)

  # NTL year range derived from the stack
  ntl_yrs      <- seq(2000, 2000 + terra::nlyr(ntl_r) - 1)
  ntl_native   <- file.path(out_dir, "ntl.tif")
  ntl_wgs      <- file.path(out_dir, "ntl_wgs84.tif")
  ntl_change   <- file.path(out_dir,
                            sprintf("ntl_change_%d_%d_wgs84.tif",
                                    min(ntl_yrs), max(ntl_yrs)))
  ntl_files    <- c(ntl_native, ntl_wgs, ntl_change)

  # Lit/unlit tri-state stack aligned to urban epochs
  lit_native   <- file.path(out_dir, "lit_unlit.tif")
  lit_wgs      <- file.path(out_dir, "lit_unlit_wgs84.tif")
  lit_files    <- c(lit_native, lit_wgs)

  do_urban <- overwrite || !all(file.exists(urban_files))
  do_ntl   <- overwrite || !all(file.exists(ntl_files))
  do_lit   <- overwrite || !all(file.exists(lit_files))
  do_ext   <- overwrite || !all(file.exists(ext_files))

  if (!do_urban && !do_ntl && !do_lit && !do_ext) return(invisible(slug))

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  yrs <- c(2000, 2005, 2010, 2015, 2020, 2025)

  # --- validate polygon once (used by both blocks) ---
  city_sf <- sf::st_as_sf(city_row) |> sf::st_make_valid()

  # ---------------- Urban block ----------------
  if (do_urban) {
    city_v <- city_sf |> terra::vect() |> terra::project(crs(total_r))

    if (!terra::relate(terra::ext(city_v), terra::ext(total_r), "intersects")) {
      message(sprintf("Skipping urban for '%s' — polygon does not overlap raster extent.",
                      city_row$agglos_name))
    } else {
      total_c <- terra::crop(total_r, city_v, mask = TRUE)
      nres_c  <- terra::crop(nres_r,  city_v, mask = TRUE)
      res_c   <- total_c - nres_c
      names(total_c) <- names(nres_c) <- names(res_c) <- yrs

      terra::writeRaster(total_c, native_files[1], overwrite = TRUE, gdal = "COMPRESS=DEFLATE")
      terra::writeRaster(nres_c,  native_files[2], overwrite = TRUE, gdal = "COMPRESS=DEFLATE")
      terra::writeRaster(res_c,   native_files[3], overwrite = TRUE, gdal = "COMPRESS=DEFLATE")

      total_w <- terra::project(total_c, "EPSG:4326")
      nres_w  <- terra::project(nres_c,  "EPSG:4326")
      res_w   <- terra::project(res_c,   "EPSG:4326")
      names(total_w) <- names(nres_w) <- names(res_w) <- yrs

      terra::writeRaster(total_w, wgs_files[1], overwrite = TRUE, gdal = "COMPRESS=DEFLATE")
      terra::writeRaster(nres_w,  wgs_files[2], overwrite = TRUE, gdal = "COMPRESS=DEFLATE")
      terra::writeRaster(res_w,   wgs_files[3], overwrite = TRUE, gdal = "COMPRESS=DEFLATE")

      terra::writeRaster(res_w[["2025"]]  - res_w[["2000"]],
                         change_files[1], overwrite = TRUE, gdal = "COMPRESS=DEFLATE")
      terra::writeRaster(nres_w[["2025"]] - nres_w[["2000"]],
                         change_files[2], overwrite = TRUE, gdal = "COMPRESS=DEFLATE")

      poly <- terra::project(city_v, "EPSG:4326") |> sf::st_as_sf()
      sf::st_write(poly, poly_file, delete_dsn = TRUE, quiet = TRUE)
    }
  }

  # ---------------- Extensive-margin block ----------------
  # Reads from the per-city WGS84 stacks already on disk — never touches the
  # continental rasters, so this block is cheap to re-run independently.
  if (do_ext && all(file.exists(wgs_files[2:3]))) {
    res_w_d  <- terra::rast(wgs_files[3])  # res_wgs84.tif
    nres_w_d <- terra::rast(wgs_files[2])  # nres_wgs84.tif
    names(res_w_d)  <- yrs
    names(nres_w_d) <- yrs
    terra::writeRaster(
      terra::ifel(res_w_d[["2000"]] == 0 & res_w_d[["2025"]] > 0, res_w_d[["2025"]], NA),
      ext_files[1], overwrite = TRUE, gdal = "COMPRESS=DEFLATE"
    )
    terra::writeRaster(
      terra::ifel(nres_w_d[["2000"]] == 0 & nres_w_d[["2025"]] > 0, nres_w_d[["2025"]], NA),
      ext_files[2], overwrite = TRUE, gdal = "COMPRESS=DEFLATE"
    )
  }

  # ---------------- NTL block ----------------
  if (do_ntl) {
    city_v_ntl <- city_sf |> terra::vect() |> terra::project(crs(ntl_r))

    if (!terra::relate(terra::ext(city_v_ntl), terra::ext(ntl_r), "intersects")) {
      message(sprintf("Skipping NTL for '%s' — polygon does not overlap NTL raster extent.",
                      city_row$agglos_name))
    } else {
      ntl_c <- terra::crop(ntl_r, city_v_ntl, mask = TRUE)
      names(ntl_c) <- ntl_yrs
      terra::writeRaster(ntl_c, ntl_native, overwrite = TRUE, gdal = "COMPRESS=DEFLATE")

      ntl_w <- terra::project(ntl_c, "EPSG:4326")
      names(ntl_w) <- ntl_yrs
      terra::writeRaster(ntl_w, ntl_wgs, overwrite = TRUE, gdal = "COMPRESS=DEFLATE")

      terra::writeRaster(ntl_w[[as.character(max(ntl_yrs))]] -
                           ntl_w[[as.character(min(ntl_yrs))]],
                         ntl_change, overwrite = TRUE, gdal = "COMPRESS=DEFLATE")
    }
  }

  # ---------------- Lit/unlit block ----------------
  # Combines per-city built-up (urban epochs) with NTL snapped to those epochs.
  # 2025 has no NTL — falls back to 2024. Encoding: 0=unbuilt, 1=built-unlit, 2=built-lit.
  if (do_lit && file.exists(native_files[1]) && file.exists(ntl_native)) {
    total_disk <- terra::rast(native_files[1])
    ntl_disk   <- terra::rast(ntl_native)

    ntl_match <- pmin(yrs, max(ntl_yrs))   # 2000..2020 unchanged, 2025 -> 2024
    if (!all(as.character(ntl_match) %in% names(ntl_disk))) {
      message(sprintf("Skipping lit/unlit for '%s' — NTL layers missing for required years.",
                      city_row$agglos_name))
    } else {
      ntl_sel    <- ntl_disk[[as.character(ntl_match)]]
      ntl_on_ghsl <- terra::project(ntl_sel, total_disk, method = "bilinear")

      built_mask <- total_disk > 0
      lit_mask   <- ntl_on_ghsl > ntl_threshold

      lit_unlit  <- built_mask * (1 + lit_mask)
      names(lit_unlit) <- yrs

      terra::writeRaster(lit_unlit, lit_native, overwrite = TRUE,
                         datatype = "INT1U", gdal = "COMPRESS=DEFLATE")

      lit_w <- terra::project(lit_unlit, "EPSG:4326", method = "near")
      names(lit_w) <- yrs
      terra::writeRaster(lit_w, lit_wgs, overwrite = TRUE,
                         datatype = "INT1U", gdal = "COMPRESS=DEFLATE")
    }
  }

  invisible(slug)
}

#Apply function
purrr::walk(
  seq_len(nrow(agglom_100)),
  \(i) process_city(agglom_100[i, ], total_africa, nres_africa, ntl_africa),
  .progress = TRUE
)