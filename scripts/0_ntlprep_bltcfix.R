# 0_ntlprep_bltcfix.R
# Prepare the bloom- + top-coding-corrected DMSP nighttime-lights series.
# Replaces the DMSP+VIIRS GEE blend (0_loadntl.R -> 0_ntlprep.R) as the app's
# NTL source.
#
# Input : data/raw/ntl-regan/bloomtopcode_fix.zip
#         -> 34 global GeoTIFFs, DMSP<year>_bltcfix.tif, year = 1992..2025,
#            EPSG:4326, 30 arc-sec (~1 km), Int32. Values are corrected/extended
#            DN (0..~1075) -- NOT nW/cm2/sr. Lit/unlit threshold is re-derived
#            downstream on this scale, not the old 0.5.
# Output: data/intermediate/raster/ntl_bltcfix_africa.tif
#         34 bands, names = years 1992..2025, cropped to the Africa bbox.
#
# vs. the old ntl_urbanafrica.tif: ~1 km (was ~500 m), 1992-2025 & 2025 native
# (was 2000-2024 with 2024 proxied for 2025), single corrected DMSP series
# (was DMSP+VIIRS harmonised).

pacman::p_load(terra, here, stringr)

raw_dir <- here("data/raw/ntl-regan")
zip_f   <- file.path(raw_dir, "bloomtopcode_fix.zip")
out_dir <- file.path(raw_dir, "bloomtopcode_fix")
if (!file.exists(zip_f)) stop("Missing ", zip_f)

dir.create(here("data/intermediate/raster"), showWarnings = FALSE, recursive = TRUE)

years <- 1992:2025

# -- 1. Unzip (skip files already extracted) --------------------------------
have <- file.path(out_dir, sprintf("DMSP%d_bltcfix.tif", years))
if (!all(file.exists(have))) {
  message("Unzipping ", basename(zip_f), " ...")
  unzip(zip_f, exdir = raw_dir)
}

# -- 2. Name-match one raster per year (never positional) -------------------
tifs <- list.files(out_dir, pattern = "DMSP\\d{4}_bltcfix\\.tif$", full.names = TRUE)
files <- vapply(years, function(y) {
  hit <- grep(sprintf("DMSP%d_bltcfix", y), tifs, value = TRUE)
  if (length(hit) != 1) stop("Expected exactly one raster for ", y, " -- found ", length(hit))
  hit
}, character(1))

ntl <- terra::rast(files)
names(ntl)    <- as.character(years)
varnames(ntl) <- "ntl_bltcfix"

# -- 3. Crop to Africa (same bbox as 0_ghslprep.R / 0_popprep.R) -----------
# Source is already EPSG:4326 -> crop directly, no reprojection.
africa_ext <- terra::ext(-26, 64, -47, 38)
ntl_africa <- terra::crop(ntl, africa_ext)
names(ntl_africa)    <- as.character(years)
varnames(ntl_africa) <- "ntl_bltcfix"

# -- 4. Save --------------------------------------------------------------
terra::writeRaster(
  ntl_africa,
  here("data/intermediate/raster/ntl_bltcfix_africa.tif"),
  overwrite = TRUE,
  gdal = "COMPRESS=DEFLATE"
)

cat(sprintf("Wrote data/intermediate/raster/ntl_bltcfix_africa.tif  (%d bands: %d-%d)\n",
            terra::nlyr(ntl_africa), min(years), max(years)))
print(ntl_africa)
