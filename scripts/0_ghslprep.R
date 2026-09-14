# 0_ghslprep.R
# Prepare the GHS-BUILT-S raster stacks: unzip, name-match by epoch, stack,
# crop to the Africa bounding box.
#
# Global rasters: https://human-settlement.emergency.copernicus.eu/download.php?ds=bu
# Product: GHS_BUILT_S, release R2023A, Mollweide (EPSG:54009), 100 m.
# Epochs: 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025 (8).
#
# Input : data/raw/ghsl/built_s/
#           GHS_BUILT_S_E<year>_GLOBE_R2023A_54009_100_V1_0.{zip,tif}       (total)
#           GHS_BUILT_S_NRES_E<year>_GLOBE_R2023A_54009_100_V1_0.{zip,tif}  (non-residential)
# Output: data/intermediate/raster/{total,nres,res}_africa.tif  (8 bands, names = epoch years)
#         res = total - nres.

#### 1. Setup ####
pacman::p_load(tidyverse, terra, sf, here)

# Cap terra's RAM budget so 8 global epochs don't OOM the 16 GB box (see
# 1_create_citydata.R). terra tiles instead of loading whole windows.
terra::terraOptions(memfrac = 0.35, progress = 0)

built_s_dir <- here("data/raw/ghsl/built_s")
if (!dir.exists(built_s_dir)) {
  stop("No GHS-BUILT-S directory at ", built_s_dir,
       "\nPlace GHS_BUILT_S_E<year>_... and GHS_BUILT_S_NRES_E<year>_... ",
       "(.zip or .tif) there for 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025.")
}

# writeRaster() does not create parent directories.
dir.create(here("data/intermediate/raster"), showWarnings = FALSE, recursive = TRUE)

years <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025)

#### 2. Unzip (only what is still missing) ####
# Each archive holds one <basename>.tif; skip archives already extracted so a
# re-run does not spend ~30 min re-inflating the 2000-2025 files.
zip_files <- list.files(built_s_dir, pattern = "\\.zip$", full.names = TRUE)
for (z in zip_files) {
  target <- sub("\\.zip$", ".tif", z)
  if (!file.exists(target)) {
    message("Unzipping ", basename(z), " ...")
    unzip(z, exdir = built_s_dir)
  }
}

#### 3. Name-match one raster per epoch (never positional indexing) ####
tifs <- list.files(built_s_dir, pattern = "\\.tif$", full.names = TRUE)
tifs <- tifs[!grepl("\\.ovr$", tifs)]

pick <- function(nres) {
  vapply(years, function(y) {
    pat <- if (nres) sprintf("GHS_BUILT_S_NRES_E%d_", y)
           else       sprintf("(?<!NRES_)GHS_BUILT_S_E%d_", y)
    hit <- grep(pat, tifs, value = TRUE, perl = TRUE)
    if (length(hit) != 1)
      stop("Expected exactly one raster matching '", pat, "' in ", built_s_dir,
           " -- found ", length(hit), ".")
    hit
  }, character(1))
}

total_raster <- terra::rast(pick(nres = FALSE))
nres_raster  <- terra::rast(pick(nres = TRUE))
names(total_raster) <- years
names(nres_raster)  <- years

#### 4. Crop to Africa ####
# Bounding box wide enough for Tunis/Algiers (north), Cape Verde (west) and
# Mauritius/Reunion (east). Reproject the bbox to GHSL native CRS before crop.
africa_bbox_sf <- st_bbox(
  c(xmin = -26, ymin = -47, xmax = 64, ymax = 38),
  crs = st_crs(4326)) |> st_as_sfc()

africa_vect <- vect(africa_bbox_sf) |> project(crs(total_raster))

total_africa <- terra::crop(total_raster, africa_vect)
nres_africa  <- terra::crop(nres_raster,  africa_vect)
names(total_africa) <- years
names(nres_africa)  <- years

# RES = TOTAL - NRES
res_africa <- total_africa - nres_africa
names(res_africa) <- years

#### 5. Save ####
out <- here("data/intermediate/raster")
terra::writeRaster(total_africa, file.path(out, "total_africa.tif"),
                   overwrite = TRUE, gdal = "COMPRESS=DEFLATE")
terra::writeRaster(nres_africa, file.path(out, "nres_africa.tif"),
                   overwrite = TRUE, gdal = "COMPRESS=DEFLATE")
terra::writeRaster(res_africa, file.path(out, "res_africa.tif"),
                   overwrite = TRUE, gdal = "COMPRESS=DEFLATE")

cat(sprintf("Wrote total/nres/res_africa.tif  (%d bands: %s)\n",
            terra::nlyr(total_africa), paste(years, collapse = ", ")))
