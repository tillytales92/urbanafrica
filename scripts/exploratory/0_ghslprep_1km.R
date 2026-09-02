# 0_ghslprep_1km.R
# Prepare 1km GHSL built-up rasters and plot a continent-wide diff layer.
# Global rasters: https://human-settlement.emergency.copernicus.eu/download.php?ds=bu

#### 1. Setup ####
pacman::p_load(
  tidyverse, terra, sf, here,
  ggplot2, scales, janitor,
  rnaturalearth, patchwork)

#### 2. List + unzip files ####
zip_files <- list.files(here("data/raw/ghsl/1km"), pattern = "\\.zip$", full.names = FALSE)
walk(zip_files, ~unzip(here("data/raw/ghsl/1km", .x), exdir = here("data/raw/ghsl/1km")))

tifs <- list.files(here("data/raw/ghsl/1km"), pattern = "\\.tif$", full.names = TRUE)

#### 3. Load rasters ####
# Match by epoch year in filename — safer than positional indexing.
tif_2000 <- tifs[grep("E2000", tifs)]
tif_2025 <- tifs[grep("E2025", tifs)]
total_raster <- terra::rast(c(tif_2000, tif_2025))
names(total_raster) <- c(2000, 2025)

#### 4. Crop to Africa ####
africa_bbox_sf <- st_bbox(
  c(xmin = -26, ymin = -47, xmax = 64, ymax = 38),
  crs = st_crs(4326)) |>
  st_as_sfc()

africa_vect   <- vect(africa_bbox_sf) |> project(crs(total_raster))
builtup_africa <- terra::crop(total_raster, africa_vect)

diff_layer <- builtup_africa[["2025"]] - builtup_africa[["2000"]]

#### 5. Low-resolution diff map ####
# Aggregate 1km → 10km before plotting — fast prototype.
# Set FACT <- 1 (no aggregation) for the final high-res version.
FACT <- 5

diff_agg <- terra::aggregate(diff_layer, fact = FACT, fun = "mean", na.rm = TRUE)
diff_wgs  <- terra::project(diff_agg, "EPSG:4326")
names(diff_wgs) <- "change"

africa_sf    <- rnaturalearth::ne_countries(
  continent   = "Africa",
  scale       = "medium",
  returnclass = "sf"
)
africa_union <- sf::st_union(africa_sf)

# Mask to land outline — drops ocean pixels within the bounding box
diff_masked <- terra::mask(diff_wgs, terra::vect(africa_union))

# Positive-only: negatives are negligible (<< 1% of pixels); zeros show as basemap.
diff_df <- as.data.frame(diff_masked, xy = TRUE) |>
  filter(!is.na(change), change > 0) |>
  mutate(change_tr = log1p(change))   # log1p compresses right skew cleanly

cap <- quantile(diff_df$change_tr, 0.99, na.rm = TRUE)

# Nice legend break values back-transformed from log scale
brk_raw   <- c(1, 10, 100, 500, 1500, 5000)
brk_tr    <- log1p(brk_raw)
brk_tr    <- brk_tr[brk_tr <= cap]
brk_raw   <- brk_raw[seq_along(brk_tr)]

ggplot() +
  # Ocean background
  theme_void(base_size = 13) +
  # Land fill (rendered before raster so growth pixels sit on top)
  geom_sf(data = africa_union, fill = "#ede8df", colour = NA) +
  # Growth raster
  geom_raster(data = diff_df,
              aes(x, y, fill = pmin(change_tr, cap))) +
  # Outer coastline only — no internal country borders
  geom_sf(data = africa_union, fill = NA, colour = "white", linewidth = 0.3) +
  scale_fill_viridis_c(
    option   = "inferno",
    limits   = c(0, cap),
    breaks   = brk_tr,
    labels   = label_comma()(brk_raw),
    name     = "Built-up change\n(m² / pixel)",
    guide    = guide_colourbar(
      barwidth      = unit(0.45, "cm"),
      barheight     = unit(4,    "cm"),
      title.hjust   = 0,
      label.theme   = element_text(size = 8, colour = "grey25"),
      title.theme   = element_text(size = 9, colour = "grey15",
                                   margin = margin(b = 4))
    )
  ) +
  coord_sf(xlim = c(-26, 64), ylim = c(-47, 38), expand = FALSE) +
  labs(
    title    = "Rapid Urbanisation in Africa",
    subtitle = paste0("Built-up Expansion 2000-2025  ·  ", FACT, " km aggregation  ·  log scale")
  ) +
  theme(
    plot.background  = element_rect(fill = "#d6e8f2", colour = NA),
    panel.background = element_rect(fill = "#d6e8f2", colour = NA),
    plot.title       = element_text(size = 16, face = "bold", colour = "grey10",
                                    margin = margin(t = 10, b = 3)),
    plot.subtitle    = element_text(size = 10, colour = "grey45",
                                    margin = margin(b = 10)),
    legend.position  = "right",
    plot.margin      = margin(6, 6, 6, 6)
  )

# ---- To go high-res: set FACT <- 1 and re-run from section 5 ----



# 9. Save raster stacks (run once) ----------------------------------------
# writeRaster(builtup_africa,
#             here("data/intermediate/raster/total_africa_1km.tif"),
#             overwrite = TRUE, gdal = "COMPRESS=DEFLATE")



