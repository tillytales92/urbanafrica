# 2_corridor_proximity.R  (exploratory / one-off)
# --------------------------------------------------------------------------
# Question: is the biggest built-up growth in the top-100 African
# agglomerations linked to proximity to a named development corridor
# (Thorn et al. 2022, African Development Corridors Database) -- and is that
# link stronger for SECONDARY cities than for each country's primate city?
# (Primate cities grow for many reasons regardless of corridors; secondary
# cities are the sharper test of whether corridor access itself matters.)
#
# Method: distance from each city's centroid to the nearest digitised
# corridor *line* (roads/railways/pipelines only -- the 'point' layer is
# ports/airports/nodes, a different question) in the native Mollweide CRS,
# joined to the existing 1990-2025 growth numbers. Primate/secondary is
# assigned within-sample: for each country, the top-100 city with the
# highest Africapolis pop2020 is "primate", every other top-100 city from
# that country is "secondary" (countries with only one top-100 city are
# primate-only, dropped from the secondary comparison group).
#
# Caveats (see eda_corridors.py's gaps_summary for the full list):
#   - Corridor-affiliated infrastructure only; general national roads/rail
#     off any named corridor are out of scope of this database.
#   - ~24% of corridor projects have no digitised geometry and are silently
#     excluded (the 'unmapped' layer).
#   - This is correlational, not causal: corridors are plausibly sited
#     *because* a city is already significant (reverse causality /
#     endogeneity), not only a cause of its growth.
#
# Consumes:
#   data/raw/devcorridors/doi_10_5061_dryad_9kd51c5hw__v20220909.zip (line layer)
#   data/intermediate/agglom_top100.gpkg   (slug, iso3, pop2020)
#   data/intermediate/city_index.Rds       (lon/lat, growth 1990-2025)
# Produces:
#   data/intermediate/corridor_proximity.Rds
#   output/exploratory/corridor_distance_vs_growth.png
# --------------------------------------------------------------------------

pacman::p_load(tidyverse, sf, here, janitor, scales, ggrepel)

dir.create(here("output/exploratory"), showWarnings = FALSE, recursive = TRUE)

# -- 1. corridor lines (read straight out of the zip, no unzip) -------------
zip <- here("data/raw/devcorridors/doi_10_5061_dryad_9kd51c5hw__v20220909.zip")
if (!file.exists(zip)) stop("Missing ", zip)
gpkg <- file.path("/vsizip", zip, "AfricanDevelopmentCorridorDatabase2022.gpkg")
corridors_raw <- st_read(gpkg, layer = "line", quiet = TRUE)
# 2 of the 87 lines are MULTICURVE geometry, which GEOS's nearest-feature
# can't handle (errors cleanly, doesn't crash). terra::vect() linearizes
# curves on read but segfaults when sf is also loaded in the same session
# (a real GDAL/GEOS interop bug on this box, not worth chasing for 2 rows) --
# simplest fix is to drop the 2 curved corridors rather than route around it.
is_curved <- st_geometry_type(corridors_raw) %in% c("MULTICURVE", "CURVE", "COMPOUNDCURVE", "CIRCULARSTRING")
if (any(is_curved)) {
  message("Dropping ", sum(is_curved), " corridor(s) with unsupported curved geometry: ",
          paste(corridors_raw$Corridor_name[is_curved], collapse = "; "))
}
corridors <- corridors_raw[!is_curved, ]

# -- 2. city locations + growth (top-100, native Mollweide for distance) ----
agglom <- st_read(here("data/intermediate/agglom_top100.gpkg"), quiet = TRUE) |>
  st_drop_geometry() |>
  select(slug, iso3, pop2020)

ci <- readRDS(here("data/intermediate/city_index.Rds")) |>
  transmute(slug, agglosname, iso3, lon, lat,
            total_km2_1990, total_km2_2025,
            delta_km2 = delta_total_km2_1990_2025,
            pct_growth = delta_total_km2_1990_2025 / total_km2_1990) |>
  left_join(agglom |> select(slug, pop2020), by = "slug")

cities <- st_as_sf(ci, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# project both to World Mollweide (the project's standard equal-area CRS)
moll <- "ESRI:54009"
corridors_m <- st_transform(corridors, moll)
cities_m    <- st_transform(cities, moll)

# -- 3. distance to nearest corridor line, per city --------------------------
nearest_ix <- st_nearest_feature(cities_m, corridors_m)
dist_m <- st_distance(cities_m, corridors_m[nearest_ix, ], by_element = TRUE)
cities$dist_corridor_km <- as.numeric(dist_m) / 1000
cities$nearest_corridor <- corridors_m$Corridor_name[nearest_ix]

# -- 4. primate vs. secondary, within the top-100 sample ---------------------
tab <- cities |>
  st_drop_geometry() |>
  group_by(iso3) |>
  mutate(
    n_in_country = n(),
    is_primate   = pop2020 == max(pop2020, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(city_role = case_when(
    is_primate               ~ "primate",
    !is_primate & n_in_country > 1 ~ "secondary",
    TRUE                      ~ NA_character_
  ))

saveRDS(tab, here("data/intermediate/corridor_proximity.Rds"))

# -- 5. report -----------------------------------------------------------------
cat(sprintf("\n==== Corridor proximity, %d cities ====\n", nrow(tab)))
cat(sprintf("Median distance to nearest corridor: %.0f km (range %.0f-%.0f)\n",
            median(tab$dist_corridor_km), min(tab$dist_corridor_km), max(tab$dist_corridor_km)))

cor_all <- cor.test(tab$dist_corridor_km, tab$pct_growth, method = "spearman")
cat(sprintf("\nAll cities: Spearman rho(distance, pct growth) = %.3f (p = %.3f)\n",
            cor_all$estimate, cor_all$p.value))

for (grp in c("primate", "secondary")) {
  d <- tab |> filter(city_role == grp)
  ct <- suppressWarnings(cor.test(d$dist_corridor_km, d$pct_growth, method = "spearman"))
  cat(sprintf("%-9s (n=%d): Spearman rho = %.3f (p = %.3f)\n",
              grp, nrow(d), ct$estimate, ct$p.value))
}

cat("\n---- Near- vs far-corridor median growth, by role (<=50km vs >50km) ----\n")
tab |>
  filter(!is.na(city_role)) |>
  mutate(near = ifelse(dist_corridor_km <= 50, "<=50km", ">50km")) |>
  group_by(city_role, near) |>
  summarise(n = n(), median_pct_growth = percent(median(pct_growth), accuracy = 1),
            median_dist_km = round(median(dist_corridor_km)), .groups = "drop") |>
  as.data.frame() |>
  print(row.names = FALSE)

cat("\n---- Top 10 fastest-growing SECONDARY cities: distance to nearest corridor ----\n")
tab |>
  filter(city_role == "secondary") |>
  arrange(desc(pct_growth)) |>
  transmute(agglosname, iso3, pct_growth = percent(pct_growth, accuracy = 1),
            dist_corridor_km = round(dist_corridor_km), nearest_corridor) |>
  head(10) |>
  as.data.frame() |>
  print(row.names = FALSE)

cat("\n---- Bottom 10 slowest-growing SECONDARY cities: distance to nearest corridor ----\n")
tab |>
  filter(city_role == "secondary") |>
  arrange(pct_growth) |>
  transmute(agglosname, iso3, pct_growth = percent(pct_growth, accuracy = 1),
            dist_corridor_km = round(dist_corridor_km), nearest_corridor) |>
  head(10) |>
  as.data.frame() |>
  print(row.names = FALSE)

# -- 6. plot -------------------------------------------------------------------
p <- ggplot(tab |> filter(!is.na(city_role)),
            aes(dist_corridor_km, pct_growth, colour = city_role)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +
  ggrepel::geom_text_repel(
    data = ~ dplyr::slice_max(.x, pct_growth, n = 8),
    aes(label = agglosname), size = 3, max.overlaps = 20, show.legend = FALSE) +
  scale_y_continuous(labels = label_percent()) +
  scale_colour_manual(values = c(primate = "#7f8c8d", secondary = "#c0392b"),
                       name = "City role") +
  labs(
    title    = "Does corridor proximity predict growth -- especially for secondary cities?",
    subtitle = "Distance to nearest development-corridor line vs. built-up growth, 1990-2025",
    x        = "Distance to nearest corridor (km)",
    y        = "Built-up growth, 1990-2025 (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave(here("output/exploratory/corridor_distance_vs_growth.png"), p,
       width = 11, height = 8, dpi = 300)

cat("\nWrote data/intermediate/corridor_proximity.Rds",
    "\n      output/exploratory/corridor_distance_vs_growth.png\n")
