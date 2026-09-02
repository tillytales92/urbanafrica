pacman::p_load(terra, dplyr, tidyr, ggplot2, scales, here)

city       <- "accra"
K          <- 6      # number of trajectory types — adjust after inspecting elbow plot
cities_dir <- here("data/intermediate/cities")

# Load all 6 epochs of total built-up (res + nres), WGS84 for display
r_tot  <- terra::rast(file.path(cities_dir, city, "total_wgs84.tif"))
epochs <- as.integer(names(r_tot))   # 2000 2005 2010 2015 2020 2025

# ── Feature matrix ─────────────────────────────────────────────────────────────
# Rows = pixels, cols = epochs. Keep only pixels that ever had any built-up.
v         <- terra::values(r_tot, na.rm = FALSE)
keep      <- rowSums(v > 0, na.rm = TRUE) > 0 & complete.cases(v)
v_built   <- v[keep, ]
cat("Built-up pixels:", nrow(v_built), "of", nrow(v), "\n")

# Flag pixels that were zero in 2000 — these are pure new land (sprawl).
# Column z-score scaling alone cannot separate them from established low-density
# pixels (both sit below the 2000 epoch mean), so we force the separation by
# adding a binary 2000-status indicator with extra weight before scaling.
new_land_flag <- as.integer(v_built[, "2000"] == 0)
cat("Pure new-land pixels (v2000=0):", sum(new_land_flag),
    sprintf("(%.1f%%)\n", mean(new_land_flag) * 100))

# Augmented feature matrix: 6 epoch values + weighted new-land indicator.
# Weight = range of epoch values so the flag has similar leverage as a full epoch.
flag_weight <- diff(range(v_built))
v_aug       <- cbind(v_built, new_land = new_land_flag * flag_weight)

# Column z-score on the augmented matrix
v_scaled <- scale(v_aug)
sds      <- attr(v_scaled, "scaled:scale")
means    <- attr(v_scaled, "scaled:center")

# ── Elbow plot: choose K at the bend ───────────────────────────────────────────
wss <- sapply(2:10, function(k) {
  kmeans(v_scaled, centers = k, nstart = 10, iter.max = 50)$tot.withinss
})
p_elbow <- ggplot(data.frame(k = 2:10, wss = wss), aes(k, wss)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 2:10) +
  labs(title   = paste0(tools::toTitleCase(city), " — elbow plot"),
       subtitle = "Pick K at the point where adding another cluster yields little gain",
       x = "Number of clusters (K)", y = "Total within-cluster sum of squares") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())
print(p_elbow)

# ── K-means ────────────────────────────────────────────────────────────────────
set.seed(42)
km <- kmeans(v_scaled, centers = K, nstart = 50)

# Back-transform centroids — full augmented matrix, then keep only epoch columns
centers_full <- sweep(sweep(km$centers, 2, sds, "*"), 2, means, "+")
centers_orig <- centers_full[, seq_along(epochs), drop = FALSE]
centers_orig[centers_orig < 0] <- 0
colnames(centers_orig) <- as.character(epochs)

# The indicator column (col 7) back-transformed tells us: clusters with a high
# value are dominated by pixels that were zero in 2000 (new land).
indicator_bt <- centers_full[, ncol(centers_full)]
new_land_cut <- 0.5 * flag_weight   # midpoint of the indicator's [0, flag_weight] range

# Thresholds from 2025 non-zero values for density classification
vals_2025 <- v_built[v_built[, "2025"] > 0, "2025"]
thresh_lo <- quantile(vals_2025, 0.25)
thresh_hi <- quantile(vals_2025, 0.75)

# ── Auto-classify trajectory type ──────────────────────────────────────────────
sizes <- as.integer(table(km$cluster)[seq_len(K)])

ct <- as.data.frame(centers_orig) |>
  dplyr::mutate(
    cluster     = factor(seq_len(K)),
    v2000       = .data[["2000"]],
    v2025       = .data[["2025"]],
    delta       = v2025 - v2000,
    is_new_land = indicator_bt > new_land_cut,
    type        = dplyr::case_when(
      is_new_land  & v2025 >= thresh_hi  ~ "new land — dense (sprawl)",
      is_new_land  & v2025 <  thresh_hi  ~ "new land — sparse (fringe sprawl)",
      !is_new_land & delta  >  thresh_lo ~ "intensification",
      !is_new_land & abs(delta) <= thresh_lo ~ "stable core",
      TRUE                               ~ "mixed / transitional"
    )
  ) |>
  dplyr::select(cluster, type, v2000, v2025, delta)

# ── MANUAL LABELS — edit after inspecting plots ────────────────────────────────
# cluster_labels <- c("1"="stable core","2"="intensification","3"="new land", ...)
# ct <- ct |> dplyr::mutate(type = cluster_labels[as.character(cluster)])
# ──────────────────────────────────────────────────────────────────────────────

sig_df <- as.data.frame(centers_orig) |>
  dplyr::mutate(cluster = factor(seq_len(K)),
                n       = sizes,
                label   = paste0("C", cluster, "  (n=", comma(n), ")")) |>
  tidyr::pivot_longer(cols      = as.character(epochs),
                      names_to  = "epoch",
                      values_to = "builtup_m2") |>
  dplyr::mutate(epoch = as.integer(epoch)) |>
  dplyr::left_join(ct |> dplyr::select(cluster, type), by = "cluster")

cat("\n── Cluster centroids (m²/pixel) ──\n")
print(round(centers_orig, 1))
cat("\n── Cluster summary ──\n")
print(ct)

# ── Plot 1: Trajectory signatures ─────────────────────────────────────────────
p_sig <- ggplot(sig_df, aes(epoch, builtup_m2, colour = label, group = label)) +
  geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.4) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_colour_viridis_d(option = "D", end = 0.9) +
  scale_x_continuous(breaks = epochs) +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  labs(
    title    = paste0(tools::toTitleCase(city), " — trajectory cluster signatures"),
    subtitle = paste0(K, " clusters | ", comma(sum(keep)), " built-up pixels"),
    x = NULL, y = "Avg built-up surface (m²/pixel)", colour = "Cluster"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "right")
print(p_sig)

# ── Plot 2: Spatial map of trajectory types ────────────────────────────────────
r_clust          <- r_tot[[1]]
values(r_clust)  <- NA_real_
values(r_clust)[keep] <- km$cluster
names(r_clust)   <- "cluster"

r_df <- as.data.frame(r_clust, xy = TRUE) |>
  dplyr::filter(!is.na(cluster)) |>
  dplyr::mutate(cluster = factor(as.integer(cluster))) |>
  dplyr::left_join(ct |> dplyr::select(cluster, type), by = "cluster")

p_map <- ggplot(r_df, aes(x, y, fill = type)) +
  geom_raster() +
  scale_fill_viridis_d(option = "D", end = 0.9) +
  coord_equal() +
  labs(
    title = paste0(tools::toTitleCase(city), " — spatial distribution of trajectory types"),
    x = NULL, y = NULL, fill = "Trajectory type"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid  = element_blank(),
        axis.text   = element_blank(),
        axis.ticks  = element_blank())
print(p_map)
