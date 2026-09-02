####Load NTL for Metropolitan Areas from GEE####
# Per-metro bbox approach: one export task per metro, 25 years stacked as
# bands. Avoids the all-of-Africa raster and the slow clip against detailed
# Africapolis polygons. Source: harmonised DMSP-OLS + VIIRS (sat-io).

# -- 1. Environment -----------------------------------------------------------
# Point RGEE_PYTHON at the Python/conda env that has the `earthengine-api`
# package (e.g. the one created by rgee::ee_install()). Set it in ~/.Renviron:
#   RGEE_PYTHON=/path/to/rgee_py/python
rgee_env_dir <- Sys.getenv("RGEE_PYTHON")
if (nzchar(rgee_env_dir)) {
  Sys.setenv(RETICULATE_PYTHON  = rgee_env_dir)
  Sys.setenv(EARTHENGINE_PYTHON = rgee_env_dir)
}

library(reticulate)

pacman::p_load(rgee, googledrive, leaflet, tidyverse, here, sf, janitor,
               terra)

# -- 2. Authenticate ----------------------------------------------------------
# EE_USER = your Earth Engine account email (optional; unset uses the default).
ee_user <- Sys.getenv("EE_USER"); if (!nzchar(ee_user)) ee_user <- NULL
rgee::ee_Authenticate()
rgee::ee_Initialize(user = ee_user, drive = TRUE)
ee_check()
googledrive::drive_auth()

sf_use_s2(FALSE)

# -- 3. Build per-metro bboxes (top 100 agglomerations by 2020 pop) ----------
agglom_top100<- st_read(here("data/raw/africapolis/agglomerations.shp"),
                         quiet = TRUE) |>
  clean_names() |>
  slice_max(pop2020, n = 100) |>
  st_make_valid()

# Per-row bbox + ~5 km buffer (0.05° at the equator) to catch peri-urban lights
buffer_deg <- 0.05
metro_bboxes <- lapply(seq_len(nrow(agglom_top100)), function(i) {
  bb <- st_bbox(agglom_top100[i, ])
  list(
    slug = agglom_top100$slug[i],
    xmin = unname(bb["xmin"]) - buffer_deg,
    ymin = unname(bb["ymin"]) - buffer_deg,
    xmax = unname(bb["xmax"]) + buffer_deg,
    ymax = unname(bb["ymax"]) + buffer_deg
  )
})

# -- 4. Build a single multi-band NTL image (one band per year) --------------
npp_viirs_ntl <- ee$ImageCollection("projects/sat-io/open-datasets/npp-viirs-ntl")

build_ntl_stack <- function(years, collection = npp_viirs_ntl) {
  bands <- lapply(years, function(y) {
    collection$
      filterDate(paste0(y, "-01-01"), paste0(y, "-12-31"))$
      mean()$
      rename(paste0("ntl_", y))
  })
  Reduce(function(a, b) a$addBands(b), bands)
}

ntl_stack <- build_ntl_stack(2000:2024)

# -- 5. Per-metro export function --------------------------------------------
export_metro_ntl <- function(bbox,
                             image  = ntl_stack,
                             folder = "GEE_Exports_NTL",
                             scale  = 500,
                             crs    = "EPSG:4326") {
  region <- ee$Geometry$Rectangle(
    coords   = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax),
    proj     = crs,
    geodesic = FALSE
  )
  task <- ee_image_to_drive(
    image          = image$clip(region),
    description    = paste0("ntl_", bbox$slug),
    folder         = folder,
    fileNamePrefix = paste0("ntl_", bbox$slug),
    region         = region,
    scale          = scale,
    crs            = crs,
    maxPixels      = 1e10
  )
  task$start()
  message("Started: ", bbox$slug)
  task
}

# -- 6. One-metro test (Accra) -----------------------------------------------
# Run this first; ee_monitoring blocks until the task finishes.
test_bbox <- metro_bboxes[[which(sapply(metro_bboxes, \(b) b$slug == "accra"))]]
task_test <- export_metro_ntl(test_bbox)
ee_monitoring(task_test)

googledrive::drive_ls("GEE_Exports_NTL")

# -- 7. Full loop (run only after the single-metro test succeeds) ------------
# 100 tasks total — well under GEE's ~3000 active task limit.
# tasks <- purrr::map(metro_bboxes, export_metro_ntl)
# purrr::walk(tasks, ee_monitoring)   # blocks; or skip and check Tasks tab

# -- 8. Sanity-check a downloaded file ---------------------------------------
# 25-band tif: band i corresponds to year 2000+i-1
ntl_accra <- terra::rast(here("data/raw/ntl/ntl_accra.tif"))
names(ntl_accra) <- paste0("ntl_", 2000:2024)
plot(ntl_accra[["ntl_2020"]])


# 9. Testing buffers ------------------------------------------------------
st_write(agglom_top100,
         dsn = paste(here(),"data","intermediate","shapefiles","agglom_top100.shp",sep = "/"))


agglom_top100_buffers

agglom_1 <- agglom_top100 |>
  slice_max(pop2020, n = 1)

agglom_top1_buffers <-
  agglom_top100_buffers |>
  slice_max(pop2020,n=1)



plot(st_geometry(agglom_top1_buffers),
     border = "black",
     col = NA)

plot(st_geometry(agglom_1),
     add = TRUE,
     border = "red",
     col = rgb(1, 0, 0, 0.3))

library(sf)
library(leaflet)

leaflet() |>
  addTiles() |>

  # Buffers
  addPolygons(
    data = agglom_top1_buffers,
    color = "blue",
    weight = 2,
    fillOpacity = 0.2,
    group = "Buffers"
  ) |>

  # Original agglomeration
  addPolygons(
    data = agglom_1,
    color = "red",
    weight = 3,
    fillOpacity = 0.4,
    group = "Agglomeration"
  ) |>

  addLayersControl(
    overlayGroups = c("Buffers", "Agglomeration"),
    options = layersControlOptions(collapsed = FALSE)
  )

