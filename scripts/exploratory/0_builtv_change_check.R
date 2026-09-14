# 0_builtv_change_check.R  (exploratory / one-off)
# --------------------------------------------------------------------------
# Question: is there enough VERTICAL growth (building volume / mean height)
# across the top-100 African agglomerations between 2000 and 2025 to justify
# a continent-wide GHS-BUILT-V prep + extraction pass (mirroring BUILT-S)?
#
# GHS-BUILT-V R2023A: total building VOLUME, m3 per 100 m cell (total and a
# separate NRES layer; RES = total - NRES). Same grid / CRS / epochs as
# BUILT-S. Mean building height over the built footprint = V / S  (m3 / m2 = m).
#
# This script does NOT crop anything to Africa. It reads the four BUILT-V
# global rasters straight out of their .zip via GDAL /vsizip/ and lets
# terra::extract() pull only the windows under the 100 city polygons. Pair
# them with the BUILT-S 2000 / 2025 bands from the already-cropped continental
# stacks so we can turn volume into mean height.
#
# Consumes:
#   data/raw/ghsl/built_v/GHS_BUILT_V_{,NRES_}E{2000,2025}_GLOBE_R2023A_54009_100_V1_0.zip
#   data/intermediate/raster/total_africa.tif   (BUILT-S total, 6 bands)
#   data/intermediate/raster/nres_africa.tif    (BUILT-S nres,  6 bands)
#   data/raw/africapolis/agglomerations.shp
# Produces:
#   data/intermediate/builtv_change_check.Rds        (per-city table)
#   output/exploratory/builtv_height_change.png      (scatter: footprint vs height growth)
# --------------------------------------------------------------------------

pacman::p_load(tidyverse, terra, sf, here, janitor, scales, ggrepel)

years <- c(2000, 2025)

vzip_dir <- here("data/raw/ghsl/built_v")
dir.create(here("output/exploratory"), showWarnings = FALSE, recursive = TRUE)

# -- 1. BUILT-V sources (read from inside the zip, no unzip) -----------------
vsizip <- function(zip) {
  inner <- sub("\\.zip$", ".tif", basename(zip))
  file.path("/vsizip", zip, inner)
}

zip_files <- c(
  v_tot_2000  = "GHS_BUILT_V_E2000_GLOBE_R2023A_54009_100_V1_0.zip",
  v_tot_2025  = "GHS_BUILT_V_E2025_GLOBE_R2023A_54009_100_V1_0.zip",
  v_nres_2000 = "GHS_BUILT_V_NRES_E2000_GLOBE_R2023A_54009_100_V1_0.zip",
  v_nres_2025 = "GHS_BUILT_V_NRES_E2025_GLOBE_R2023A_54009_100_V1_0.zip"
)
zips <- setNames(file.path(vzip_dir, zip_files), names(zip_files))

if (!all(file.exists(zips))) {
  stop("Missing BUILT-V zip(s):\n  ",
       paste(names(zips)[!file.exists(zips)], collapse = "\n  "))
}
vpaths <- vapply(zips, vsizip, character(1))

v_tot  <- terra::rast(unname(vpaths[c("v_tot_2000",  "v_tot_2025")]))
v_nres <- terra::rast(unname(vpaths[c("v_nres_2000", "v_nres_2025")]))
names(v_tot) <- names(v_nres) <- as.character(years)

# -- 2. BUILT-S 2000 / 2025 bands from the continental crops ----------------
s_tot_stack  <- terra::rast(here("data/intermediate/raster/total_africa.tif"))
s_nres_stack <- terra::rast(here("data/intermediate/raster/nres_africa.tif"))

s_tot  <- s_tot_stack[[as.character(years)]]
s_nres <- s_nres_stack[[as.character(years)]]
names(s_tot) <- names(s_nres) <- as.character(years)

# -- 3. Top-100 agglomerations (canonical set from 0_simplifyshapefile.R) -------
gpkg <- here("data/intermediate/agglom_top100.gpkg")
if (!file.exists(gpkg)) stop("Missing ", gpkg, " -- run scripts/0_simplifyshapefile.R first.")
agglom_100 <- st_read(gpkg, quiet = TRUE)

vect_moll <- vect(agglom_100) |> project(crs(v_tot))

# -- 4. Zonal sums ------------------------------------------------------------
zsum <- function(r, prefix) {
  terra::extract(r, vect_moll, fun = sum, na.rm = TRUE, ID = FALSE) |>
    as_tibble() |>
    rename_with(~ paste0(prefix, "_", .x))
}

message("Extracting BUILT-V total ...");  z_v_tot  <- zsum(v_tot,  "v_tot")
message("Extracting BUILT-V nres  ...");  z_v_nres <- zsum(v_nres, "v_nres")
message("Extracting BUILT-S total ...");  z_s_tot  <- zsum(s_tot,  "s_tot")
message("Extracting BUILT-S nres  ...");  z_s_nres <- zsum(s_nres, "s_nres")

# -- 5. Assemble per-city table --------------------------------------------------
# Units: V in m3, S in m2. Volume reported in millions of m3 (Mm3), footprint
# in km2, mean height V/S in metres. RES = total - NRES.
tab <- bind_cols(
  agglom_100 |> st_drop_geometry() |> transmute(id, agglosname = agglos_name, iso3, pop2020),
  z_v_tot, z_v_nres, z_s_tot, z_s_nres
) |>
  mutate(
    v_res_2000  = v_tot_2000  - v_nres_2000,
    v_res_2025  = v_tot_2025  - v_nres_2025,
    s_res_2000  = s_tot_2000  - s_nres_2000,
    s_res_2025  = s_tot_2025  - s_nres_2025,

    # mean height over the built footprint (m)
    h_tot_2000  = v_tot_2000  / s_tot_2000,
    h_tot_2025  = v_tot_2025  / s_tot_2025,
    h_res_2000  = v_res_2000  / s_res_2000,
    h_res_2025  = v_res_2025  / s_res_2025,
    h_nres_2000 = v_nres_2000 / s_nres_2000,
    h_nres_2025 = v_nres_2025 / s_nres_2025,

    # deltas
    d_h_tot     = h_tot_2025 - h_tot_2000,
    d_h_res     = h_res_2025 - h_res_2000,
    d_h_nres    = h_nres_2025 - h_nres_2000,
    pct_h_tot   = d_h_tot / h_tot_2000,

    v_tot_2000_Mm3 = v_tot_2000 / 1e6,
    v_tot_2025_Mm3 = v_tot_2025 / 1e6,
    pct_v_tot   = (v_tot_2025 - v_tot_2000) / v_tot_2000,

    s_tot_2000_km2 = s_tot_2000 / 1e6,
    s_tot_2025_km2 = s_tot_2025 / 1e6,
    pct_s_tot   = (s_tot_2025 - s_tot_2000) / s_tot_2000,

    # of the total volume added, how much came from a taller footprint vs a
    # wider one?  vertical share = 1 - (footprint growth / volume growth)
    vertical_share = 1 - pct_s_tot / pct_v_tot
  ) |>
  arrange(desc(d_h_tot))

saveRDS(tab, here("data/intermediate/builtv_change_check.Rds"))

# -- 6. Report -----------------------------------------------------------------
summ <- tab |>
  summarise(
    n                 = n(),
    med_h2000         = median(h_tot_2000, na.rm = TRUE),
    med_h2025         = median(h_tot_2025, na.rm = TRUE),
    med_d_h           = median(d_h_tot,    na.rm = TRUE),
    med_pct_h         = median(pct_h_tot,  na.rm = TRUE),
    med_pct_v         = median(pct_v_tot,  na.rm = TRUE),
    med_pct_s         = median(pct_s_tot,  na.rm = TRUE),
    n_gain_gt_0_5m    = sum(d_h_tot > 0.5, na.rm = TRUE),
    n_gain_gt_1m      = sum(d_h_tot > 1.0, na.rm = TRUE),
    n_gain_gt_2m      = sum(d_h_tot > 2.0, na.rm = TRUE),
    med_vertical_share = median(vertical_share, na.rm = TRUE)
  )

cat("\n==== BUILT-V 2000 -> 2025, top-100 African agglomerations ====\n")
print(as.data.frame(summ), digits = 3)

cat("\n---- Top 15 by absolute mean-height gain (m) ----\n")
tab |>
  transmute(agglosname, iso3,
            h_2000 = round(h_tot_2000, 2),
            h_2025 = round(h_tot_2025, 2),
            d_h    = round(d_h_tot, 2),
            pct_v  = percent(pct_v_tot, accuracy = 1),
            pct_s  = percent(pct_s_tot, accuracy = 1),
            vert_share = percent(vertical_share, accuracy = 1)) |>
  head(15) |>
  as.data.frame() |>
  print(row.names = FALSE)

cat("\n---- Bottom 10 by mean-height gain ----\n")
tab |>
  transmute(agglosname, iso3,
            h_2000 = round(h_tot_2000, 2),
            h_2025 = round(h_tot_2025, 2),
            d_h    = round(d_h_tot, 2)) |>
  tail(10) |>
  as.data.frame() |>
  print(row.names = FALSE)

# -- 7. Scatter: footprint growth vs height growth --------------------------
p <- ggplot(tab, aes(pct_s_tot, d_h_tot)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey60") +
  geom_point(aes(size = v_tot_2025_Mm3), alpha = 0.6, colour = "#2980b9") +
  ggrepel::geom_text_repel(
    data = ~ dplyr::slice_max(.x, d_h_tot, n = 12),
    aes(label = agglosname), size = 3, max.overlaps = 20) +
  scale_x_continuous(labels = label_percent()) +
  scale_size_continuous(name = "Building volume\n2025 (Mm³)", labels = label_comma()) +
  labs(
    title    = "Vertical vs. horizontal growth, 2000–2025",
    subtitle = "Top-100 African agglomerations · GHS-BUILT-V / BUILT-S R2023A",
    x        = "Footprint growth (BUILT-S %)",
    y        = "Change in mean building height (m)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave(here("output/exploratory/builtv_height_change.png"), p,
       width = 11, height = 8, dpi = 300)

cat("\nWrote data/intermediate/builtv_change_check.Rds",
    "\n      output/exploratory/builtv_height_change.png\n")
