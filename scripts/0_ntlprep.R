#Here:Combine NTL rasters which where downloaded in GEE
#Load NTL
pacman::p_load(rgee, googledrive, leaflet, tidyverse, here, sf, janitor,
               terra)

#load the NTL files
tifs <- list.files(path = here("data/raw/ntl"), pattern = "\\.tif$", full.names = TRUE)

#Load all — tiles have non-overlapping extents, so build a SpatRasterCollection
#and merge into a single SpatRaster covering the union of their extents.
ntl_collection <- terra::sprc(lapply(tifs, terra::rast))
ntl_raster <- terra::merge(ntl_collection)

#test plot
plot(ntl_raster[[22]])

#adjust names
names(ntl_raster) <- seq(2000,2024,1)
varnames(ntl_raster) <- "ntl"

# Write raster ------------------------------------------------------------
writeRaster(ntl_raster,filename = paste(here(),
            "data/intermediate/raster/ntl_urbanafrica.tif",sep = "/"))
