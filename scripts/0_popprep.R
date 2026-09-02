# 0_popprep.R
# Prepare the GHS-POP raster stack: unzip, read, name-match by epoch, crop to Africa.
# Mirrors 0_ghslprep.R but for residential population (persons per 100 m cell).
# Global rasters: https://human-settlement.emergency.copernicus.eu/download.php?ds=pop
# Product: GHS_POP, release R2023A, Mollweide (EPSG:54009), 100 m, epochs 2000-2025.
#
# Output: data/intermediate/raster/pop_africa.tif  (6 bands, names = epoch years)

#### 1. Setup ####
pacman::p_load(tidyverse, terra, sf, here)

pop_dir <- here("data/raw/ghsl/pop")
if (!dir.exists(pop_dir)) {
  stop("No GHS-POP directory at ", pop_dir,
       "\nDownload GHS_POP_E<year>_GLOBE_R2023A_54009_100_V1_0.tif for ",
       "2000, 2005, 2010, 2015, 2020, 2025 and place them (or their .zip) there.")
}

# writeRaster() does not create parent directories.
dir.create(here("data/intermediate/raster"), showWarnings = FALSE, recursive = TRUE)

years <- c(2000, 2005, 2010, 2015, 2020, 2025)

#### 2. List + unzip files ####
zip_files <- list.files(pop_dir, pattern = "\\.zip$", full.names = FALSE)
walk(zip_files, ~ unzip(file.path(pop_dir, .x), exdir = pop_dir))

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

pop_africa <- terra::crop(pop_raster, africa_vect)
names(pop_africa)    <- years
varnames(pop_africa) <- "pop"

# sanity check
plot(pop_africa[["2025"]])

#### 5. Save ####
terra::writeRaster(
  pop_africa,
  filename  = here("data/intermediate/raster/pop_africa.tif"),
  overwrite = TRUE,
  gdal      = "COMPRESS=DEFLATE"
)

cat("\nWrote data/intermediate/raster/pop_africa.tif  (",
    terra::nlyr(pop_africa), " bands: ", paste(years, collapse = ", "), ")\n", sep = "")
