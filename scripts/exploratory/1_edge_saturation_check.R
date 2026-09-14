# 1_edge_saturation_check.R  (exploratory / one-off)
# --------------------------------------------------------------------------
# Question: for how many of the 100 cities is the Africapolis (2015-vintage,
# fixed) boundary itself the constraint on measured growth, rather than the
# city having genuinely slowed down?
#
# We cannot check for missed growth by buffering OUTWARD past the boundary --
# the ring just outside a coastal or lakeside city is often open water, so a
# "no built-up out there" reading would be a false negative, not evidence the
# boundary wasn't binding. Instead we look INWARD: erode each city's polygon
# by EDGE_WIDTH_M to get a thin ring running just inside the existing border,
# and compare its 2025 built-up density to the interior's. A ring that is
# nearly as built-up as the core means the city has pressed right up against
# its frame -- its measured growth is a floor, not the true figure. A ring
# that is still mostly open land means the boundary wasn't binding and the
# growth number can be trusted. This only ever looks at pixels already inside
# the existing polygon, so it needs no land/water mask.
#
# Consumes:
#   data/intermediate/agglom_top100.gpkg   (slugs)
#   data/intermediate/cities/<slug>/metro.gpkg   (native Mollweide polygon)
#   data/intermediate/cities/<slug>/total.tif    (native Mollweide, 8 epochs)
#   data/intermediate/city_index.Rds       (delta_total_km2_1990_2025, for the cross-check)
# Produces:
#   data/intermediate/edge_saturation_check.Rds
#   output/exploratory/edge_saturation_vs_growth.png
# --------------------------------------------------------------------------

pacman::p_load(tidyverse, terra, sf, here, janitor, scales, ggrepel)

EDGE_WIDTH_M <- 1500   # ring width, metres (Mollweide is metric/equal-area)

dir.create(here("output/exploratory"), showWarnings = FALSE, recursive = TRUE)

gpkg <- here("data/intermediate/agglom_top100.gpkg")
if (!file.exists(gpkg)) stop("Missing ", gpkg, " -- run scripts/0_simplifyshapefile.R first.")
slugs <- st_read(gpkg, quiet = TRUE)$slug

edge_fill_for_city <- function(slug) {
  city_dir <- here("data/intermediate/cities", slug)
  poly_f   <- file.path(city_dir, "metro.gpkg")
  rast_f   <- file.path(city_dir, "total.tif")
  if (!file.exists(poly_f) || !file.exists(rast_f)) {
    return(tibble(slug = slug, note = "missing per-city assets"))
  }

  r <- terra::rast(rast_f)[["2025"]]
  v <- terra::vect(poly_f) |> terra::project(terra::crs(r))

  interior <- terra::buffer(v, width = -EDGE_WIDTH_M)
  if (is.null(interior) || nrow(interior) == 0 || terra::expanse(interior, unit = "km") <= 0) {
    return(tibble(slug = slug, note = "polygon too small/thin to erode -- skipped"))
  }
  edge_ring <- terra::erase(v, interior)

  built_edge_km2     <- as.numeric(terra::extract(r, edge_ring, fun = sum, na.rm = TRUE, ID = FALSE)) / 1e6
  built_interior_km2 <- as.numeric(terra::extract(r, interior,  fun = sum, na.rm = TRUE, ID = FALSE)) / 1e6
  edge_area_km2      <- terra::expanse(edge_ring, unit = "km")
  interior_area_km2  <- terra::expanse(interior,  unit = "km")

  tibble(
    slug              = slug,
    edge_area_km2     = edge_area_km2,
    interior_area_km2 = interior_area_km2,
    edge_fill         = built_edge_km2 / edge_area_km2,
    interior_fill     = built_interior_km2 / interior_area_km2,
    note              = NA_character_
  )
}

# Resumable: a prior run's saved table is reused for slugs that already scored
# cleanly (note is NA); only missing/failed slugs are (re)computed. A few very
# complex multi-part coastline/delta polygons (Cairo, Nairobi, Kigali, ...)
# need up to ~150s -- 60s was too tight and cut them off mid-buffer.
out_rds <- here("data/intermediate/edge_saturation_check.Rds")
prior <- if (file.exists(out_rds)) readRDS(out_rds) else NULL
already_ok <- if (!is.null(prior)) prior$slug[is.na(prior$note) & !is.na(prior$edge_fill)] else character(0)
todo <- setdiff(slugs, already_ok)

message("Computing edge-vs-interior built-up fill for ", length(todo), "/", length(slugs),
        " cities (", length(already_ok), " reused from a prior run) ...")
results <- vector("list", length(todo))
for (i in seq_along(todo)) {
  t0 <- Sys.time()
  setTimeLimit(elapsed = 240, transient = TRUE)
  results[[i]] <- tryCatch(
    edge_fill_for_city(todo[i]),
    error = function(e) tibble(slug = todo[i], note = paste("error/timeout:", conditionMessage(e)))
  )
  setTimeLimit(elapsed = Inf, transient = TRUE)
  message(sprintf("  %3d/%d  %-25s %.1fs", i, length(todo), todo[i],
                   as.numeric(Sys.time() - t0, units = "secs")))
}
new_tab <- bind_rows(results)
tab <- if (!is.null(prior)) {
  bind_rows(prior |> filter(slug %in% already_ok), new_tab)
} else {
  new_tab
}
tab <- tab |>
  mutate(
    edge_to_interior_ratio = edge_fill / interior_fill,
    boundary_saturated     = edge_to_interior_ratio >= 0.9,
    low_signal             = edge_fill < 0.10 & interior_fill < 0.10
  )

# -- cross-check against measured growth -------------------------------------
ci <- readRDS(here("data/intermediate/city_index.Rds")) |>
  transmute(slug, agglosname, iso3,
            total_km2_1990, total_km2_2025,
            pct_growth_1990_2025 = delta_total_km2_1990_2025 / total_km2_1990)

tab <- tab |>
  select(slug, edge_area_km2, interior_area_km2, edge_fill, interior_fill,
         edge_to_interior_ratio, boundary_saturated, low_signal, note) |>
  left_join(ci, by = "slug") |>
  arrange(desc(edge_to_interior_ratio))

saveRDS(tab, out_rds)

# -- report --------------------------------------------------------------------
n_ok      <- sum(!is.na(tab$edge_fill))
n_flagged <- sum(tab$boundary_saturated & !tab$low_signal, na.rm = TRUE)
cat(sprintf("\n==== Edge-saturation check, %d/%d cities scored ====\n", n_ok, nrow(tab)))
cat(sprintf("Flagged boundary-saturated (edge/interior fill ratio >= 0.9, excluding low-density noise): %d cities\n", n_flagged))

cat("\n---- Top 15 most boundary-saturated ----\n")
tab |>
  filter(!is.na(edge_fill)) |>
  transmute(agglosname, iso3,
            edge_fill     = percent(edge_fill, accuracy = 1),
            interior_fill = percent(interior_fill, accuracy = 1),
            ratio         = round(edge_to_interior_ratio, 2),
            pct_growth    = percent(pct_growth_1990_2025, accuracy = 1)) |>
  head(15) |>
  as.data.frame() |>
  print(row.names = FALSE)

cat("\n---- 15 least boundary-constrained (most slack inside the frame) ----\n")
tab |>
  filter(!is.na(edge_fill)) |>
  arrange(edge_to_interior_ratio) |>
  transmute(agglosname, iso3,
            edge_fill     = percent(edge_fill, accuracy = 1),
            interior_fill = percent(interior_fill, accuracy = 1),
            ratio         = round(edge_to_interior_ratio, 2),
            pct_growth    = percent(pct_growth_1990_2025, accuracy = 1)) |>
  head(15) |>
  as.data.frame() |>
  print(row.names = FALSE)

if (any(!is.na(tab$note) & tab$note != "")) {
  cat("\n---- Skipped / problem cities ----\n")
  tab |> filter(!is.na(note)) |> select(slug, note) |> as.data.frame() |> print(row.names = FALSE)
}

# -- does boundary saturation correlate with LOOKING like it slowed down? ----
p <- ggplot(tab |> filter(!is.na(edge_fill)),
            aes(edge_to_interior_ratio, pct_growth_1990_2025)) +
  geom_vline(xintercept = 0.9, linewidth = 0.3, colour = "grey60", linetype = "dashed") +
  geom_point(aes(colour = boundary_saturated), alpha = 0.7, size = 2) +
  ggrepel::geom_text_repel(
    data = ~ dplyr::slice_max(.x, edge_to_interior_ratio, n = 10),
    aes(label = agglosname), size = 3, max.overlaps = 20) +
  scale_y_continuous(labels = label_percent()) +
  scale_colour_manual(values = c(`TRUE` = "#c0392b", `FALSE` = "#2980b9"),
                       name = "Boundary-\nsaturated") +
  labs(
    title    = "Is the boundary itself capping measured growth?",
    subtitle = "Edge-ring vs. interior built-up fill (2025) vs. measured growth 1990–2025",
    x        = "Edge / interior fill ratio (1.0 = ring as built-up as the core)",
    y        = "Measured built-up growth, 1990–2025 (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave(here("output/exploratory/edge_saturation_vs_growth.png"), p,
       width = 11, height = 8, dpi = 300)

cat("\nWrote data/intermediate/edge_saturation_check.Rds",
    "\n      output/exploratory/edge_saturation_vs_growth.png\n")
