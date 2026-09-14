# 0_popprep.R
# Prepare the GHS-POP raster stack: unzip, read, name-match by epoch, crop to Africa.
# Mirrors 0_ghslprep.R but for residential population (persons per 100 m cell).
# Global rasters: https://human-settlement.emergency.copernicus.eu/download.php?ds=pop
# Product: GHS_POP, release R2023A, Mollweide (EPSG:54009), 100 m.
# Epochs: 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025 (8) — aligned with 0_ghslprep.R.
#
# Output: data/intermediate/raster/pop_africa.tif  (8 bands, names = epoch years)

#### 1. Setup ####
pacman::p_load(tidyverse, terra, sf, here)

terra::terraOptions(progress = 0)
# NB: do NOT lower memfrac here. terra::crop over 8 stacked ~5 GB global rasters
# picks its block size from the RAM budget; the default (0.6) keeps blocks large
# and the crop sequential. A low cap (e.g. 0.35) tiles GHS-POP's dense Float32
# grid into thousands of tiny read/write cycles and the crop effectively never
# finishes. The original 2000-2025 run used the default and completed fine.

pop_dir <- here("data/raw/ghsl/pop")
if (!dir.exists(pop_dir)) {
  stop("No GHS-POP directory at ", pop_dir,
       "\nDownload GHS_POP_E<year>_GLOBE_R2023A_54009_100_V1_0.tif for ",
       "1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025 and place them (or their .zip) there.")
}

# writeRaster() does not create parent directories.
dir.create(here("data/intermediate/raster"), showWarnings = FALSE, recursive = TRUE)

years <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025)

#### 2. List + unzip files ####
# Skip archives already extracted — the GHS-POP zips are 4-5 GB each.
zip_files <- list.files(pop_dir, pattern = "\\.zip$", full.names = TRUE)
for (z in zip_files) {
  target <- sub("\\.zip$", ".tif", z)
  if (!file.exists(target)) {
    message("Unzipping ", basename(z), " ...")
    unzip(z, exdir = pop_dir)
  }
}

#### 3. Name-match one 100 m raster per epoch ####
# Match by epoch string in the filename -- never positional indexing (see CLAUDE.md).
tifs <- list.files(pop_dir, pattern = "GHS_POP_E\\d{4}.*_100_.*\\.tif$", full.names = TRUE)

pop_files <- vapply(years, function(y) {
  hit <- grep(sprintf("GHS_POP_E%d_", y), tifs, value = TRUE)
  if (length(hit) != 1) {
    stop("Expected exactly one 100 m GHS_POP raster for ", y,
         " in ", pop_dir, " -- found ", length(hit), ".")
  }
  hit
}, character(1))

pop_raster <- terra::rast(pop_files)
names(pop_raster)    <- years
varnames(pop_raster) <- "pop"

#### 4. Crop to Africa ####
# Same bounding box as 0_ghslprep.R (wide enough for Tunis/Algiers, Cape Verde,
# Mauritius/Reunion). Reproject the bbox to GHS-POP native CRS before cropping.
africa_bbox_sf <- st_bbox(
  c(xmin = -26, ymin = -47, xmax = 64, ymax = 38),
  crs = st_crs(4326)) |>
  st_as_sfc()

africa_vect <- vect(africa_bbox_sf) |> project(crs(pop_raster))

#### 5. Crop, then write compressed — exactly as the known-good 2000-2025 run ####
# Two passes: crop to an (uncompressed) temp raster, then one sequential
# writeRaster with plain DEFLATE. Do NOT add PREDICTOR / TILED / ZLEVEL: the
# source is Float64 (FLT8S) and GDAL's float predictor bloats it ~7x (a 25 GB
# runaway was observed). Do NOT compress inside crop(filename=) either — it
# interleaves the 8-file windowed reads with DEFLATE and crawls. terra's temp
# dir is on %TEMP%, outside the Dropbox/OneDrive tree.
out_file <- here("data/intermediate/raster/pop_africa.tif")

pop_africa <- terra::crop(pop_raster, africa_vect)
names(pop_africa)    <- years
varnames(pop_africa) <- "pop"

terra::writeRaster(
  pop_africa, out_file, overwrite = TRUE,
  gdal = c("COMPRESS=DEFLATE", "BIGTIFF=IF_SAFER")
)

cat("\nWrote ", out_file, "  (", terra::nlyr(pop_africa), " bands: ",
    paste(years, collapse = ", "), ")\n", sep = "")
