# 2_1_timeseries.R
# Time-series ggplots for African metros — residential vs. non-residential built-up,
# plus nighttime lights and unlit share.
# Consumes:
#   data/intermediate/africapolis_builtup.Rds  (from 1_extract_ghsl.R § 5)
#   data/intermediate/africapolis_ntl.Rds      (from 1_extract_ntl.R)

# ── 1. Setup ──────────────────────────────────────────────────────────────────
pacman::p_load(tidyverse, here, scales)

dir.create(here("output"), showWarnings = FALSE)

builtup_path <- here("data/intermediate/africapolis_builtup.Rds")
if (!file.exists(builtup_path)) {
  stop("Missing ", builtup_path,
       " — run scripts/1_extract_ghsl.R through section 5 first.")
}
ts_wide <- readRDS(builtup_path)

target_years <- c(2000, 2005, 2010, 2015, 2020, 2025)

type_palette <- c(
  "Total"           = "#2c3e50",
  "Residential"     = "#e74c3c",
  "Non-Residential" = "#2980b9"
)

# Long form: one row per (city, year, type) for res / nres lines
ts_long <- ts_wide |>
  dplyr::select(id, agglosname, iso3, year,
         Residential     = area_res_km2,
         `Non-Residential` = area_nres_km2,
         Total           = area_total_km2) |>
  pivot_longer(c(Residential, `Non-Residential`, Total),
               names_to = "type", values_to = "area_km2")

# Rank metros by absolute total built-up increase 2000 → 2025
metro_growth <- ts_wide |>
  filter(year %in% c(2000, 2025)) |>
  dplyr::select(id, agglosname, iso3, year, area_total_km2) |>
  pivot_wider(names_from = year, values_from = area_total_km2,
              names_prefix = "y") |>
  mutate(delta_km2 = y2025 - y2000,
         pct_growth = if_else(y2000 > 0, (y2025 - y2000) / y2000, NA_real_)) |>
  arrange(desc(delta_km2))

saveRDS(metro_growth, here("data/intermediate/africapolis_metro_growth.Rds"))

# ── 2. Top-16 metros — res vs. nres area over time (faceted) ─────────────────
top_cities <- metro_growth |> slice_head(n = 16) |> pull(agglosname)

p_res_nres <- ts_long |>
  filter(agglosname %in% top_cities,
         type %in% c("Residential", "Non-Residential")) |>
  mutate(label = paste0(agglosname, " (", iso3, ")"),
         label = fct_reorder(label, -area_km2, .fun = max)) |>
  ggplot(aes(year, area_km2, colour = type, group = type)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~ label, ncol = 4, scales = "free_y") +
  scale_colour_manual(values = type_palette) +
  scale_x_continuous(breaks = target_years) +
  scale_y_continuous(labels = label_number(suffix = " km²", big.mark = ",")) +
  labs(
    title    = "Built-Up Area Over Time — Top 16 African Metros",
    subtitle = "GHSL R2023 (Africapolis, pop2020 > 3M); ranked by absolute total growth 2000–2025",
    x        = NULL,
    y        = "Built-up area",
    colour   = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank(),
        strip.text       = element_text(face = "bold"))

ggsave(here("output/africapolis_res_nres_top16.png"),
       p_res_nres, width = 13, height = 9, dpi = 300)

# ── 3. Top-16 metros — 5-year change in res / nres ───────────────────────────
ts_change <- ts_long |>
  filter(agglosname %in% top_cities,
         type %in% c("Residential", "Non-Residential")) |>
  arrange(agglosname, type, year) |>
  group_by(agglosname, iso3, type) |>
  mutate(delta_km2 = area_km2 - lag(area_km2)) |>
  ungroup() |>
  filter(!is.na(delta_km2)) |>
  mutate(label = paste0(agglosname, " (", iso3, ")"),
         label = fct_reorder(label, -delta_km2, .fun = max))

p_change <- ggplot(ts_change, aes(factor(year), delta_km2, fill = type)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  facet_wrap(~ label, ncol = 4, scales = "free_y") +
  scale_fill_manual(values = type_palette) +
  scale_y_continuous(labels = label_number(suffix = " km²", big.mark = ",")) +
  labs(
    title    = "5-Year Change in Built-Up Area — Top 16 African Metros",
    subtitle = "Residential vs. Non-Residential, GHSL R2023",
    x        = "Period end year",
    y        = "Change in area",
    fill     = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank(),
        strip.text       = element_text(face = "bold"))

ggsave(here("output/africapolis_res_nres_change_top16.png"),
       p_change, width = 13, height = 9, dpi = 300)

# ── 4. Continent-wide distribution of non-residential share by year ──────────
p_dist <- ts_wide |>
  filter(!is.na(share_nres)) |>
  ggplot(aes(factor(year), share_nres)) +
  geom_boxplot(outlier.size = 0.6, fill = "#2980b9", alpha = 0.4) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  labs(
    title    = "Non-Residential Built-Up Share Across African Metros",
    subtitle = "Africapolis (pop2020 > 3M); each box = all metros for that year",
    x        = NULL,
    y        = "Non-residential share"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

ggsave(here("output/africapolis_nres_share_distribution.png"),
       p_dist, width = 10, height = 6, dpi = 300)

# ── 5. NTL plots — annual, 2000–2024 ─────────────────────────────────────────
ntl_path <- here("data/intermediate/africapolis_ntl.Rds")
if (!file.exists(ntl_path)) {
  warning("Missing ", ntl_path,
          " — skipping NTL plots. Run scripts/1_extract_ntl.R first.")
} else {
  ntl_long <- readRDS(ntl_path) |>
    mutate(unlit_share = 1 - ntl_lit_share,
           label       = paste0(agglosname, " (", iso3, ")"))

  # Same facet ordering as the built-up plots, restricted to top_cities
  ntl_top <- ntl_long |>
    filter(agglosname %in% top_cities) |>
    mutate(label = factor(label,
                          levels = unique(label[order(match(agglosname, top_cities))])))

  ntl_year_breaks <- c(2000, 2005, 2010, 2015, 2020, 2024)

  # 5a. Mean NTL radiance over time
  p_ntl_mean <- ggplot(ntl_top, aes(year, ntl_mean)) +
    geom_line(linewidth = 0.9, colour = "#d35400") +
    geom_point(size = 1.6, colour = "#d35400") +
    facet_wrap(~ label, ncol = 4, scales = "free_y") +
    scale_x_continuous(breaks = ntl_year_breaks) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    labs(
      title    = "Nighttime Lights Over Time — Top 16 African Metros",
      subtitle = "Mean NTL radiance per agglomeration (harmonised DMSP-OLS + VIIRS, 2000–2024)",
      x        = NULL,
      y        = "Mean NTL (nW/cm²/sr)"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          strip.text       = element_text(face = "bold"))

  ggsave(here("output/africapolis_ntl_mean_top16.png"),
         p_ntl_mean, width = 13, height = 9, dpi = 300)

  # 5b. Share of unlit area over time
  p_unlit <- ggplot(ntl_top, aes(year, unlit_share)) +
    geom_line(linewidth = 0.9, colour = "#34495e") +
    geom_point(size = 1.6, colour = "#34495e") +
    facet_wrap(~ label, ncol = 4, scales = "free_y") +
    scale_x_continuous(breaks = ntl_year_breaks) +
    scale_y_continuous(labels = label_percent(accuracy = 1)) +
    labs(
      title    = "Unlit Share Over Time — Top 16 African Metros",
      subtitle = "Share of agglomeration pixels below the NTL threshold (≈0.5 nW/cm²/sr)",
      x        = NULL,
      y        = "Unlit share"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          strip.text       = element_text(face = "bold"))

  ggsave(here("output/africapolis_unlit_share_top16.png"),
         p_unlit, width = 13, height = 9, dpi = 300)

  # 5c. Continent-wide distribution of unlit share by year
  p_unlit_dist <- ntl_long |>
    filter(!is.na(unlit_share)) |>
    ggplot(aes(factor(year), unlit_share)) +
    geom_boxplot(outlier.size = 0.6, fill = "#34495e", alpha = 0.4) +
    scale_y_continuous(labels = label_percent(accuracy = 1)) +
    labs(
      title    = "Unlit Share Across African Metros",
      subtitle = "Africapolis top-100; each box = all metros for that year",
      x        = NULL,
      y        = "Unlit share"
    ) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank())

  ggsave(here("output/africapolis_unlit_share_distribution.png"),
         p_unlit_dist, width = 11, height = 6, dpi = 300)
}

cat("\nDone. Plots written to output/\n")
