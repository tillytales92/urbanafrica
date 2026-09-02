#0_prep
#prepare the GHSL Raster layers: Unzip raster files, read in, stack and crop to Africa extent
#Global Raster Layers where downloaded here:https://human-settlement.emergency.copernicus.eu/download.php?ds=bu
#Ran 1-5; not sure if 6-7 are needed: take very long time to calculate and might be too large for shiny
#### 1. Setup ####
pacman::p_load(
  tidyverse, terra, sf, here,
  ggplot2, scales,janitor)

#### 2. List Files####
#List all zip files in folder and unzip
zip_files <- list.files(path = here("data/raw/ghsl"), pattern = "\\.zip$", full.names = FALSE)
#unzip:this takes a while since files are huge
walk(zip_files, ~unzip(here("data/raw/ghsl", .x), exdir = here("data/raw/ghsl")))

#load the NRES GHSL rasters for each year (2000,2005,2010,2015,2020,2025: 6 in total)
tifs <- list.files(path = here("data/raw/ghsl"), pattern = "\\.tif$", full.names = TRUE)

#### 3. Load Raster stacks####
#filter for total built-up: RES + NRES
total_raster <- terra::rast(tifs[1:6])

#filter for NRES
nres_raster <- terra::rast(tifs[7:12])

#### 4. Cropping####
#Africa bounding box (wide enough to include Tunis/Algiers in the north,
# Cape Verde in the west, and Mauritius/Réunion in the east)
africa_bbox_sf <- st_bbox(
  c(xmin = -26, ymin = -47, xmax = 64, ymax = 38),
  crs = st_crs(4326)) |> st_as_sfc()

#NRES Africa
# Reproject bbox to match GHSL native CRS for masking
africa_vect <- vect(africa_bbox_sf) |> project(crs(nres_raster))

#crop and mask
nres_africa <- nres_raster |>
  terra::crop(africa_vect)

#adjust raster names
names(nres_africa) <- c(2000,2005,2010,2015,2020,2025)

#plot
plot(nres_africa[[1]])

#Total GHSL
total_africa <- total_raster |>
  terra::crop(africa_vect)

#plot
plot(total_africa[[1]])

#change names
names(total_africa) <- c(2000,2005,2010,2015,2020,2025)

#create RES stack (TOTAL - NRES)
res_africa <- total_africa - nres_africa

# 5. Save Raster stack ----------------------------------------------------
#save NRES raster stack: 2000,2005,2010,2015,2020,2025
writeRaster(nres_africa,filename = paste(here(),
            "data/intermediate/raster/nres_africa.tif",sep = "/"))

#save RES raster stack
writeRaster(res_africa,filename = paste(here(),
            "data/intermediate/raster/res_africa.tif",sep = "/"))

#save TOTAL raster stack
writeRaster(total_africa,filename = paste(here(),
            "data/intermediate/raster/total_africa.tif",sep = "/"))

# # 6.Create Metropolitan Area Raster -------------------------------------
# #crop the three rasters to metropolitan areas
# #raster objects
# #TOTAL
# total_africa <- terra::rast(here("data/intermediate/total_africa.tif"))
# #NRES
# nres_africa <- terra::rast(here("data/intermediate/nres_africa.tif"))
# names(nres_africa) <- c(2000,2005,2010,2015,2020,2025)
# #RES
# res_africa <- terra::rast(here("data/intermediate/res_africa.tif"))
#
# # Urban areas in Africa above 100,000 pop.
# agglom <- st_read(here("data/raw/africapolis/agglomerations.shp")) |>
#   clean_names() |>
#   filter(pop2020 > 100000)
#
# #filter further to test:largest 100 cities
# agglom_sel <- agglom |>
#   slice_max(pop2020,n = 100) |>
#   st_make_valid()
#
# #project
# agglom_vect <- vect(agglom_sel) |> terra::project(crs(nres_africa))
#
# #plot simplified version
# agglom_simple <- simplifyGeom(agglom_vect, tolerance = 100)
# plot(agglom_simple, border = "red", col = NA)
#
# #crop
# nres_metro <- nres_africa |> terra::crop(agglom_vect,mask = TRUE)
# res_metro <- res_africa |> crop(agglom_vect,mask = TRUE)
# total_metro <- total_africa |> crop(agglom_vect,mask = TRUE)
#
# # 7. Save Metropolitan raster stacks --------------------------------------
# #save NRES raster stack: 2000,2005,2010,2015,2020,2025
# writeRaster(nres_metro,filename = paste(here(),
#                                          "data/intermediate/nres_metro.tif",sep = "/"))
#
# #save RES raster stack
# writeRaster(res_metro,filename = paste(here(),
#                                         "data/intermediate/res_metro.tif",sep = "/"))
#
# #save TOTAL raster stack
# writeRaster(total_metro,filename = paste(here(),
#                                           "data/intermediate/total_metro.tif",sep = "/"))



