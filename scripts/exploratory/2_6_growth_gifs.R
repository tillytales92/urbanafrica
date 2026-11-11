# 2_6_growth_gifs.R
# Sentinel-2 true-colour animated GIFs, 2016-2025, for three cities chosen as
# characteristic examples of the growth-form typology (sec 8, docs/insights-1990-2025.md).
#   ikorodu -- sprawl-dominant & accelerating (26.5 -> 95.7 km2, +261%,
#              1990-2025; a Lagos exurb, same "sprawling & accelerating"
#              cluster as Lagos itself but small enough to actually see)
#   tunis   -- infill-dominant & decelerating (cleanest infill case; kept --
#              its growth is genuinely subtle, which is itself the point of
#              contrast against ikorodu)
#   bamako  -- steady & mixed (+87%, same tag as Cairo but at 1/6th the area)
# For a blogpost -- not part of the pipeline, one-off.
#
# NOTE on an earlier version: it used Sodo, not Ikorodu, as the sprawl example
# (the single biggest % grower in the sample, +580%). That looked nearly empty
# on screen -- Sodo's Africapolis polygon is a *dispersed* rural-urban complex
# (built-up pixels fill only ~1.1% of its own bounding box, same pattern as
# Nakuru/Hawassa/Embu), not a compact town, so there's no visible "blob" to
# animate no matter the zoom. Ikorodu's built-up fills ~12% of its bbox
# (vs. Tunis 2.9%, Bamako 8.9%) -- a real, compact, growing town. Bboxes are
# now each city's *built-up pixel* bbox (trimmed from total_wgs84.tif's 2025
# band), not the raw Africapolis polygon bbox, which is what caused the
# problem above.
#
# Output: output/exploratory/gifs/<slug>_sentinel_growth.gif

# -- 1. Environment -----------------------------------------------------------

rgee_env_dir <- Sys.getenv("RGEE_PYTHON")
if (nzchar(rgee_env_dir)) {
  Sys.setenv(RETICULATE_PYTHON  = rgee_env_dir)
  Sys.setenv(EARTHENGINE_PYTHON = rgee_env_dir)
}

library(reticulate)
pacman::p_load(rgee, tidyverse, here)

# -- 2. Authenticate ----------------------------------------------------------

rgee::ee_Authenticate()
rgee::ee_Initialize()
ee_check()

# -- 3. Study cities -----------------------------------------------------------

pacman::p_load(terra)

slugs <- c("ikorodu", "tunis", "bamako")

# Bbox from the actual built-up pixels (trimmed from the per-city 2025 band),
# not the raw Africapolis polygon bbox -- for some agglomerations (Sodo,
# Nakuru, Hawassa, Embu) the polygon is a dispersed rural-urban complex far
# larger than the built-up footprint itself, which left no visible "city" in
# the frame no matter how tight the padding.
built_up_bbox <- function(slug) {
  r <- terra::rast(here(file.path("data/intermediate/cities", slug, "total_wgs84.tif")))
  r2025 <- r[["2025"]]
  r2025[r2025 == 0] <- NA
  e <- terra::ext(terra::trim(r2025))
  c(e$xmin, e$ymin, e$xmax, e$ymax)
}

pad_bbox <- function(bb, factor = 0.45) {
  dx <- (bb[3] - bb[1]) * factor
  dy <- (bb[4] - bb[2]) * factor
  c(bb[1] - dx, bb[2] - dy, bb[3] + dx, bb[4] + dy)
}

# -- 4. Cloud masking + annual median composites -------------------------------

# SCL-band mask: drop cloud shadow (3), medium/high-probability cloud (8, 9),
# thin cirrus (10).
mask_s2_clouds <- function(image) {
  scl  <- image$select("SCL")
  mask <- scl$neq(3)$And(scl$neq(8))$And(scl$neq(9))$And(scl$neq(10))
  image$updateMask(mask)
}

# One cloud-free-ish true-colour composite per calendar year.
make_composite <- function(year, bounds) {
  ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
    filterBounds(bounds)$
    filterDate(paste0(year, "-01-01"), paste0(year, "-12-31"))$
    filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", 60))$
    map(mask_s2_clouds)$
    select(c("B4", "B3", "B2"))$
    median()$
    clip(bounds)
}

# Sentinel-2 full continuity starts 2016 (S2B joined S2A in 2017, so 2016
# composites may be thinner); native resolution for B4/B3/B2 is 10 m.
years <- 2016:2025

# -- 5. Build + download one GIF per city --------------------------------------

out_dir <- here("output/exploratory/gifs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (slug in slugs) {
  bb   <- pad_bbox(built_up_bbox(slug))
  rect <- ee$Geometry$Rectangle(coords = bb, proj = "EPSG:4326", geodesic = FALSE)

  cat(sprintf("%s: building %d annual composites...\n", slug, length(years)))
  composites <- ee$ImageCollection$fromImages(
    lapply(years, function(y) make_composite(y, rect))
  )

  gif_params <- list(
    region          = rect,
    dimensions      = 768,
    crs             = "EPSG:3857",
    framesPerSecond = 0.5, # was 2, then 1 -- each frame now holds for 2s (20s total)
    bands           = c("B4", "B3", "B2"),
    min             = 0,
    max             = 3000,
    gamma           = 1.3
  )

  url  <- composites$getVideoThumbURL(gif_params)
  dest <- file.path(out_dir, paste0(slug, "_sentinel_growth.gif"))
  download.file(url, dest, mode = "wb", quiet = TRUE)
  cat(sprintf("  -> %s\n", dest))
}
