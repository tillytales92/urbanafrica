# Build the canonical top-100 African agglomeration boundary set + a slim
# attribute table, both consumed downstream.
#
# Outputs:
#   data/intermediate/agglom_top100.gpkg  -- repaired top-100 polygons, WGS84
#     (id, slug, agglos_name, iso3, pop2020, p_tree_cov + geometry). Every
#     extract script reads THIS instead of re-slicing the 8,784-feature raw
#     shapefile and re-running st_make_valid().
#   data/intermediate/agglom_attrs.Rds    -- slug / tree-cover / pop2020, for the Shiny app.
#
# Why the repair matters: data/raw/africapolis/agglomerations.shp has broken
# polygon geometry -- inverted ring winding (~11 of the top 100, incl. Cairo,
# Ibadan, Durban) and degenerate/duplicate-vertex rings (Tunis, Alger, Oran,
# Constantine, Tanger). terra::vect() reports 0/100 valid. Consequences before
# repair: terra::extract(sum) summed the wrong region for the wound-backwards
# polygons and returned NaN for the degenerate ones (that's why those cities
# had NaN built-up in africapolis_builtup.Rds while still drawing on the map).
# GDAL autocorrects ring winding on read; terra::makeValid() (GEOS) fixes the
# rest -- 100/100 valid, no row-count change, no GEOMETRYCOLLECTION explosion
# that the sf make_valid + collection_extract + cast chain produced.
#
# Kisumu is excluded: Africapolis defines "Kisumu" as a ~15.5M-person,
# ~21,000 km2 mega-agglomeration (the dense Lake Victoria settlement
# continuum), not a coherent metro -- too large to be meaningful in the app.
# Dropping it lets the next-ranked agglomeration take the 100th slot. This
# exclusion is documented in the app's About / methodology tab.

pacman::p_load(here, terra, janitor, dplyr)

shp <- here("data/raw/africapolis/agglomerations.shp")

# -- read (GDAL autocorrects ring winding) + normalise attribute names --------
v <- terra::vect(shp)
names(v) <- janitor::make_clean_names(names(v))

stopifnot(all(c("agglos_name", "iso3", "pop2020", "id") %in% names(v)))

# -- top-100 by Africapolis 2020 population, with two exclusions --------------
#   Kisumu:        Africapolis "Kisumu" is a ~15.5M / ~21,000 km2 mega-blob
#                  (Lake Victoria settlement continuum), not a coherent metro.
#   Port Harcourt: the bloom/top-code-corrected DMSP NTL series (bltcfix) is
#                  identically 0 over its whole footprint in every year -- the
#                  paper's Niger-Delta gas-flare masking wipes it out, so every
#                  NTL-derived measure (lit / dim / unlit share) is unusable.
# Each exclusion lets the next-ranked agglomeration take the freed slot.
exclude <- c("Kisumu", "Port harcourt")
v <- v[order(-v$pop2020), ]
v <- v[!(v$agglos_name %in% exclude), ]
v100 <- terra::makeValid(v[seq_len(100), ])

v100$slug <- janitor::make_clean_names(v100$agglos_name)
keep <- intersect(c("id", "slug", "agglos_name", "iso3", "pop2020", "p_tree_cov"),
                  names(v100))
v100 <- v100[, keep]

# -- verify ------------------------------------------------------------------
vm  <- terra::project(v100, "ESRI:54009")
a   <- terra::expanse(vm, unit = "km")
n_bad <- sum(!terra::is.valid(v100))
if (n_bad > 0 || any(a <= 0)) {
  stop(sprintf("repair incomplete: %d invalid, %d non-positive area", n_bad, sum(a <= 0)))
}

# -- write -----------------------------------------------------------------
terra::writeVector(v100, here("data/intermediate/agglom_top100.gpkg"), overwrite = TRUE)

as.data.frame(v100) |>
  dplyr::select(slug, p_tree_cov, pop2020) |>
  saveRDS(here("data/intermediate/agglom_attrs.Rds"))

cat(sprintf(
  "Wrote data/intermediate/agglom_top100.gpkg  (%d polygons, %d/%d valid, areas %.0f-%.0f km2)\n",
  nrow(v100), sum(terra::is.valid(v100)), nrow(v100), min(a), max(a)))
cat("      data/intermediate/agglom_attrs.Rds\n")
cat(sprintf("Kisumu excluded; smallest agglomeration now in set: %s (pop2020 %s)\n",
            v100$agglos_name[which.min(v100$pop2020)],
            format(round(min(v100$pop2020)), big.mark = ",")))
