# Per-city raster splitting (for Shiny / per-metro leaflet) -----------------
# Pre-cuts the continental stacks into one folder per agglomeration and
# pre-computes everything the leaflet/Shiny layer needs at runtime:
#   data/intermediate/cities/<slug>/
#     metro.gpkg                              validated polygon, WGS84
#     {total,nres,res}.tif                    native-CRS stacks
#     {total,nres,res}_wgs84.tif              WGS84 stacks (for leaflet)
#     {res,nres}_change_1990_2025_wgs84.tif   1990→2025 diff (for leaflet)
#     pop.tif / pop_wgs84.tif                 GHS-POP stack (persons / 100 m cell)
#     pop_change_1990_2025_wgs84.tif          1990→2025 population diff (for leaflet)
#     ntl.tif / ntl_wgs84.tif                 nighttime-lights stack
#     ntl_change_<y0>_<y1>_wgs84.tif          first→last year NTL diff
#     lit_unlit.tif / lit_unlit_wgs84.tif     0=unbuilt, 1=built-unlit, 2=built-dim, 3=built-bright
#                                             (corrected-DN NTL snapped to urban epochs; native to 2025)
# Downstream scripts read these small files directly — no reprojection,
# no st_make_valid, no raster algebra at map-build time.
#
# Epoch years are read from the continental stack band names, so this script
# handles the 6-epoch (2000–2025) and the extended 8-epoch (1990–2025) sets
# alike. The headline diff/extensive-margin layers span 1990→2025 (the first
# available epoch to the last), selected by band name.

#### 1. Setup ####
pacman::p_load(
  tidyverse, terra, sf, here,
  ggplot2, scales,janitor)

# Cap terra's RAM budget. Default memfrac (0.6 of TOTAL RAM) over-commits on a
# 16 GB box with other apps open and the OS kills the process. Keep it well
# under free RAM so terra tiles instead. Run this script in batches too
# (see the CLI args at the bottom).
terra::terraOptions(memfrac = 0.35, progress = 0)

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
#NTL — bloom + top-coding corrected DMSP series (Chiovelli et al. 2026),
# annual 1992-2025, ~1 km, band names = years. Values are corrected DN, not nW.
ntl_africa <- terra::rast(here("data/intermediate/raster/ntl_bltcfix_africa.tif"))
#POP — GHS-POP R2023A, persons per 100 m cell, same Mollweide grid + epochs as
# the built-up stack. Optional: skipped per-city if the raster is absent.
pop_path   <- here("data/intermediate/raster/pop_africa.tif")
pop_africa <- if (file.exists(pop_path)) terra::rast(pop_path) else NULL
if (is.null(pop_africa)) message("NOTE: ", pop_path, " not found — population layers will be skipped.")

#Canonical top-100 agglomerations (repaired geometry + Kisumu excluded;
# see scripts/0_simplifyshapefile.R)
agglom_100 <- st_read(here("data/intermediate/agglom_top100.gpkg"), quiet = TRUE)

#Processing function
process_city <- function(city_row, total_r, nres_r, ntl_r, pop_r = NULL,
                         out_root = cities_root, overwrite = FALSE,
                         dim_max = 10) {   # built-up light classes: unlit DN=0, dim 0<DN<=dim_max, bright DN>dim_max
  slug    <- janitor::make_clean_names(city_row$agglos_name)
  out_dir <- file.path(out_root, slug)

  poly_file    <- file.path(out_dir, "metro.gpkg")
  native_files <- file.path(out_dir, c("total.tif", "nres.tif", "res.tif"))
  wgs_files    <- file.path(out_dir, c("total_wgs84.tif", "nres_wgs84.tif", "res_wgs84.tif"))
  change_files <- file.path(out_dir, c("res_change_1990_2025_wgs84.tif",
                                       "nres_change_1990_2025_wgs84.tif"))
  ext_files    <- file.path(out_dir, c("res_ext_1990_2025_wgs84.tif",
                                       "nres_ext_1990_2025_wgs84.tif"))
  urban_files  <- c(poly_file, native_files, wgs_files, change_files)

  # Population (GHS-POP) per-city files
  pop_native   <- file.path(out_dir, "pop.tif")
  pop_wgs      <- file.path(out_dir, "pop_wgs84.tif")
  pop_change   <- file.path(out_dir, "pop_change_1990_2025_wgs84.tif")
  pop_files    <- c(pop_native, pop_wgs, pop_change)

  # NTL year range read from the stack's band names (bltcfix series: 1992-2025)
  ntl_yrs      <- as.integer(names(ntl_r))
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
  # pop stack must carry the same epochs as the built-up stack, else skip.
  do_pop   <- !is.null(pop_r) && terra::nlyr(pop_r) == terra::nlyr(total_r) &&
              (overwrite || !all(file.exists(pop_files)))

  if (!do_urban && !do_ntl && !do_lit && !do_ext && !do_pop) return(invisible(slug))

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  # Epoch years from the continental stack's band names (expects the 8-epoch
  # 1990-2025 set). The headline diff / extensive-margin / pop-change layers
  # span 1990->2025 and are selected by band name below.
  yrs <- as.integer(names(total_r))
  stopifnot(!anyNA(yrs), all(c(1990, 2025) %in% yrs))

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

      terra::writeRaster(res_w[["2025"]]  - res_w[["1990"]],
                         change_files[1], overwrite = TRUE, gdal = "COMPRESS=DEFLATE")
      terra::writeRaster(nres_w[["2025"]] - nres_w[["1990"]],
                         change_files[2], overwrite = TRUE, gdal = "COMPRESS=DEFLATE")

      poly <- terra::project(city_v, "EPSG:4326") |> sf::st_as_sf()
      sf::st_write(poly, poly_file, delete_dsn = TRUE, quiet = TRUE)
    }
  }

  # ---------------- Population block (GHS-POP) ----------------
  # GHS-POP is persons per 100 m cell on the same Mollweide grid and epochs as
  # the built-up stack, so a polygon sum = total population (no /1e6). Mirrors
  # the urban block: native stack + WGS84 stack (display) + 1990->2025 diff.
  # The native pop.tif is authoritative for any downstream sums; pop_wgs84.tif
  # is display-only (reprojection does not preserve per-cell counts exactly).
  if (do_pop) {
    city_v_pop <- city_sf |> terra::vect() |> terra::project(crs(pop_r))

    if (!terra::relate(terra::ext(city_v_pop), terra::ext(pop_r), "intersects")) {
      message(sprintf("Skipping population for '%s' — polygon does not overlap POP raster extent.",
                      city_row$agglos_name))
    } else {
      pop_c <- terra::crop(pop_r, city_v_pop, mask = TRUE)
      names(pop_c) <- yrs
      terra::writeRaster(pop_c, pop_native, overwrite = TRUE, gdal = "COMPRESS=DEFLATE")

      pop_w <- terra::project(pop_c, "EPSG:4326")
      names(pop_w) <- yrs
      terra::writeRaster(pop_w, pop_wgs, overwrite = TRUE, gdal = "COMPRESS=DEFLATE")

      terra::writeRaster(pop_w[["2025"]] - pop_w[["1990"]],
                         pop_change, overwrite = TRUE, gdal = "COMPRESS=DEFLATE")
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
      terra::ifel(res_w_d[["1990"]] == 0 & res_w_d[["2025"]] > 0, res_w_d[["2025"]], NA),
      ext_files[1], overwrite = TRUE, gdal = "COMPRESS=DEFLATE"
    )
    terra::writeRaster(
      terra::ifel(nres_w_d[["1990"]] == 0 & nres_w_d[["2025"]] > 0, nres_w_d[["2025"]], NA),
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

  # ---------------- Lit / dim / bright block ----------------
  # Combines per-city built-up (urban epochs) with NTL snapped to those epochs.
  # The bltcfix series is annual 1992-2025, so every urban epoch (incl. 2025)
  # has a native NTL layer — no proxy.
  # Encoding: 0=unbuilt, 1=built-unlit (DN=0), 2=built-dim (0<DN<=dim_max),
  #           3=built-bright (DN>dim_max). On this bloom/top-code-corrected DMSP
  #           series near-all large-agglomeration built-up is DN>0 by 2025, so a
  #           bare lit/unlit split saturates; dim_max=10 (DMSP "marginal light"
  #           range, native DN 0-63) keeps a comparable, declining "dark-or-dim"
  #           share (~44% -> ~14% of built-up, 2000->2025, top-100).
  if (do_lit && file.exists(native_files[1]) && file.exists(ntl_native)) {
    total_disk <- terra::rast(native_files[1])
    ntl_disk   <- terra::rast(ntl_native)

    # Snap each urban epoch to an available NTL year: clamp into [min,max] of the
    # NTL series (1992-2025). 2000..2025 are all present; only the extended
    # 1990/1995 epochs get nudged (1990 -> 1992, 1995 -> 1995).
    ntl_match <- pmin(pmax(yrs, min(ntl_yrs)), max(ntl_yrs))
    if (!all(as.character(ntl_match) %in% names(ntl_disk))) {
      message(sprintf("Skipping lit/unlit for '%s' — NTL layers missing for required years.",
                      city_row$agglos_name))
    } else {
      ntl_sel    <- ntl_disk[[as.character(ntl_match)]]
      # Nearest-neighbour: NTL is genuinely ~1 km. Bilinear onto the 100 m GHSL
      # grid would invent sub-km precision and fractional DN.
      ntl_on_ghsl <- terra::project(ntl_sel, total_disk, method = "near")

      built_mask <- total_disk > 0
      dn_cls     <- terra::ifel(ntl_on_ghsl > dim_max, 3L,
                                terra::ifel(ntl_on_ghsl > 0, 2L, 1L))  # 1 unlit / 2 dim / 3 bright

      lit_unlit  <- built_mask * dn_cls
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
# CLI args (all optional, positional):
#   [from] [to]        process only rows from..to (1-based, inclusive) — lets the
#                      100-city rebuild run in memory-friendly batches:
#                        Rscript scripts/1_create_citydata.R 1 20
#   [... "keep"]       as a 3rd arg (or the only arg): overwrite = FALSE, i.e.
#                      build only cities whose files are missing, skip the rest.
#                      Use after a small city-set change (e.g. one swap) to avoid
#                      re-cutting the 99 unchanged cities.
# No args = full rebuild of all rows (overwrite = TRUE).
.raw   <- commandArgs(trailingOnly = TRUE)
.keep  <- "keep" %in% .raw
.nums  <- suppressWarnings(as.integer(.raw[.raw != "keep"]))
.idx   <- if (length(.nums) == 2 && all(!is.na(.nums))) {
  seq(.nums[1], min(.nums[2], nrow(agglom_100)))
} else {
  seq_len(nrow(agglom_100))
}
message(sprintf("Processing rows %d..%d of %d  (overwrite = %s)",
                min(.idx), max(.idx), nrow(agglom_100), !.keep))

purrr::walk(
  .idx,
  \(i) process_city(agglom_100[i, ], total_africa, nres_africa, ntl_africa, pop_africa,
                    overwrite = !.keep),
  .progress = TRUE
)