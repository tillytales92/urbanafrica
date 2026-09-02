# 1_extract_pop.R
# Extract GHS-POP residential population per Africapolis agglomeration.
# Mirrors 1_extract_ghsl.R / 1_extract_ntl.R.
#
# GHS-POP pixel value = number of residents in a 100 m x 100 m cell. The raster is
# in equal-area Mollweide (EPSG:54009), so summing pixels inside a polygon gives
# that polygon's total population directly -- no unit conversion.
#
# Consumes:
#   data/intermediate/raster/pop_africa.tif        (from 0_popprep.R)
#   data/raw/africapolis/agglomerations.shp
# Produces:
#   data/intermediate/africapolis_pop.Rds          (long: id, agglosname, iso3, year, pop)

# -- 1. Setup ----------------------------------------------------------------
pacman::p_load(tidyverse, terra, sf, here, janitor)

pop_path <- here("data/intermediate/raster/pop_africa.tif")
if (!file.exists(pop_path)) {
  stop("Missing ", pop_path, " -- run scripts/0_popprep.R first.")
}

years <- c(2000, 2005, 2010, 2015, 2020, 2025)

# -- 2. Load Africapolis shapefile -----------------------------------------------
# Top-100 agglomerations by 2020 population -- the canonical city set used by
# 1_extract_ntl.R and 1_create_citydata.R. (1_extract_ghsl.R still uses a
# pop2020 > 100000 filter; unifying the two is a separate cleanup.)
agglom <- st_read(here("data/raw/africapolis/agglomerations.shp")) |>
  clean_names()

agglom_100 <- agglom |>
  slice_max(pop2020, n = 100) |>
  st_make_valid()

# -- 3. Raster -----------------------------------------------------------------
pop_africa <- terra::rast(pop_path)
names(pop_africa) <- years

# -- 4. Extract per-city population ------------------------------------------
# Project polygons into the GHS-POP native CRS for extraction.
agglom_vect <- vect(agglom_100) |> project(crs(pop_africa))

pop_by_city <- terra::extract(pop_africa, agglom_vect,
                              fun = sum, na.rm = TRUE, ID = FALSE) |>
  as_tibble() |>
  mutate(id         = agglom_100$id,
         agglosname = agglom_100$agglos_name,
         iso3       = agglom_100$iso3) |>
  pivot_longer(cols = all_of(as.character(years)),
               names_to = "year", values_to = "pop") |>
  mutate(year = as.integer(year)) |>
  dplyr::select(id, agglosname, iso3, year, pop)

# -- 5. Save -----------------------------------------------------------------
saveRDS(pop_by_city, here("data/intermediate/africapolis_pop.Rds"))

cat("\nWrote data/intermediate/africapolis_pop.Rds  (",
    dplyr::n_distinct(pop_by_city$agglosname), " cities x ",
    length(years), " epochs )\n", sep = "")
