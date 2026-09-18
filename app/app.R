# app/app.R
# Urban Africa Shiny app — built-up growth maps + NTL + time series.
#
# Launch:  shiny::runApp(here::here("app"), launch.browser = TRUE)

pacman::p_load(shiny, bslib, leaflet, sf, here, dplyr, tidyr, countrycode, ggplot2,
               scales, forcats, terra, DT, plotly, shinycssloaders,
               patchwork, raster)   # leaflet::addRasterImage needs RasterLayer

# Relative paths work both locally (Shiny sets WD to app/) and on Posit Connect.
# app/data/cities is a junction → data/intermediate/cities; rsconnect follows it and bundles all rasters.
cities_root <- {
  if (dir.exists("data/cities")) "data/cities"
  else tryCatch(here::here("data/intermediate/cities"), error = function(e) NULL)
}

city_index   <- readRDS("data/city_index.Rds") |>
  dplyr::mutate(
    country    = countrycode::countrycode(iso3, "iso3c", "country.name"),
    # Five macro regions from countrycode's region23, ordered N→W→C→E→S.
    macro_region = factor(dplyr::recode(
        countrycode::countrycode(iso3, "iso3c", "region23"),
        "Northern Africa" = "Northern Africa",
        "Western Africa"  = "West Africa",
        "Middle Africa"   = "Central Africa",
        "Eastern Africa"  = "East Africa",
        "Southern Africa" = "Southern Africa"),
      levels = c("Northern Africa", "West Africa", "Central Africa",
                 "East Africa", "Southern Africa")),
    pct_growth = delta_total_km2_1990_2025 / total_km2_1990
  ) |>
  dplyr::arrange(country, agglosname)

# Join tree cover % (pre-built by 0_simplifyshapefile.R)
tree_cover   <- readRDS("data/agglom_attrs.Rds")
sprawl_stats <- readRDS("data/sprawl_stats.Rds")
city_index <- city_index |>
  dplyr::left_join(tree_cover,   by = "slug") |>
  dplyr::left_join(
    sprawl_stats |> dplyr::select(slug, sprawl_km2, intens_km2, sprawl_share,
                                   footprint_1990_km2, footprint_2025_km2,
                                   density_1990, density_2025, density_change),
    by = "slug"
  )

# Boundary-saturation flag (scripts/exploratory/1_edge_saturation_check.R): is a
# city's Africapolis polygon already built-up right up to its edge, so its
# measured growth may be capped by the fixed 2020-vintage boundary rather than a
# genuine slowdown? "saturated" excludes low_signal cities (edge_fill < 10%,
# e.g. Hawassa) where a high ratio is just noise on a near-empty polygon, not a
# real saturation signal. 10 untested cities (Cairo among them — polygon too
# geometrically complex to buffer) read "not scored".
edge_sat <- readRDS("data/edge_saturation_check.Rds") |>
  dplyr::transmute(
    slug,
    boundary_quality = dplyr::case_when(
      is.na(edge_to_interior_ratio)        ~ "Not scored",
      boundary_saturated & !low_signal     ~ "Boundary-saturated",
      TRUE                                 ~ "OK"
    )
  )
city_index <- city_index |> dplyr::left_join(edge_sat, by = "slug")

# Growth-timing & growth-form tags — dynamic tercile classification of the same
# early/late acceleration and sprawl-share metrics used in
# docs/insights-1990-2025.md §3/§4, so the app narrative tracks the write-up.
# Both require a minimum base (20 km2 in 1990 for timing; 30 km2 total growth
# for form) so small/noisy agglomerations aren't force-classified.
builtup_wide <- readRDS("data/africapolis_builtup.Rds") |>
  dplyr::select(agglosname, iso3, year, area_total_km2) |>
  tidyr::pivot_wider(names_from = year, values_from = area_total_km2, names_prefix = "y") |>
  dplyr::inner_join(city_index |> dplyr::select(slug, iso3, agglosname), by = c("iso3", "agglosname")) |>
  dplyr::transmute(
    slug,
    g_early = (y2005 - y1990) / y1990,
    g_late  = (y2025 - y2010) / y2010,
    accel   = g_late - g_early,
    base_1990 = y1990
  )
accel_terc <- stats::quantile(
  builtup_wide$accel[builtup_wide$base_1990 > 20], probs = c(1/3, 2/3), na.rm = TRUE
)
builtup_wide <- builtup_wide |>
  dplyr::mutate(
    growth_timing = dplyr::case_when(
      base_1990 <= 20      ~ "N/A (small base)",
      accel >= accel_terc[2] ~ "Accelerating",
      accel <= accel_terc[1] ~ "Decelerating",
      TRUE                  ~ "Steady"
    )
  )
city_index <- city_index |> dplyr::left_join(
  builtup_wide |> dplyr::select(slug, growth_timing), by = "slug"
)

form_terc <- stats::quantile(
  city_index$sprawl_share[city_index$sprawl_km2 + city_index$intens_km2 > 30],
  probs = c(1/3, 2/3), na.rm = TRUE
)
city_index <- city_index |>
  dplyr::mutate(
    growth_form = dplyr::case_when(
      is.na(sprawl_share) | sprawl_km2 + intens_km2 <= 30 ~ "N/A (low growth)",
      sprawl_share >= form_terc[2]                          ~ "Sprawl-dominant",
      sprawl_share <= form_terc[1]                          ~ "Infill-dominant",
      TRUE                                                  ~ "Mixed"
    )
  )

country_choices <- {
  cm <- city_index |>
    dplyr::distinct(country, iso3) |>
    dplyr::group_by(country) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::arrange(country)
  flags <- vapply(
    countrycode::countrycode(cm$iso3, "iso3c", "iso2c"),
    function(x) if (is.na(x)) "" else intToUtf8(0x1F1A5 + utf8ToInt(toupper(x))),
    character(1)
  )
  setNames(cm$country, paste(flags, cm$country))
}
default_country <- if ("Ethiopia" %in% country_choices) "Ethiopia" else country_choices[1]

cities_for <- function(country) {
  sub <- city_index |> dplyr::filter(country == !!country)
  setNames(sub$slug, sub$agglosname)
}

default_city <- {
  eth_cities <- cities_for(default_country)
  addis_idx  <- grep("addis", names(eth_cities), ignore.case = TRUE)
  if (length(addis_idx) > 0) eth_cities[[addis_idx[1]]] else eth_cities[[1]]
}

# Tidy time-series of res / nres / total km² per city-year (100 cities × 6 yrs).
ts_data <- readRDS("data/africapolis_builtup.Rds") |>
  dplyr::inner_join(
    city_index |> dplyr::select(slug, iso3, agglosname),
    by = c("iso3", "agglosname")
  )

# GHS-POP population time-series (100 cities × 8 epochs, 1990–2025).
pop_ts_data <- readRDS("data/africapolis_pop.Rds") |>
  dplyr::inner_join(
    city_index |> dplyr::select(slug, iso3, agglosname),
    by = c("iso3", "agglosname")
  )

# NTL time-series. Bloom- + top-coding-corrected DMSP (Chiovelli et al. 2026),
# annual 1992–2025; values are corrected DN, not radiance.
#   ntl_lit_share    = share of metro pixels with DN > 0 — saturates toward 1 for
#                      large agglomerations by 2025, so not used on its own.
#   ntl_dimlit_share = share with 0 < DN <= 10 (DMSP "marginal light").
#   darkdim_share    = share with DN <= 10 (dark OR dim) — the informative,
#                      non-saturating "under-lit" measure.
ntl_ts_data <- readRDS("data/africapolis_ntl.Rds") |>
  dplyr::inner_join(
    city_index |> dplyr::select(slug, iso3, agglosname),
    by = c("iso3", "agglosname")
  ) |>
  dplyr::mutate(darkdim_share = (1 - ntl_lit_share) + ntl_dimlit_share)

# Pre-compute key NTL statistics and join into city_index for info boxes.
ntl_stats_2000 <- ntl_ts_data |>
  dplyr::filter(year == 2000) |>
  dplyr::select(slug, ntl_mean_2000 = ntl_mean)
ntl_stats_2020 <- ntl_ts_data |>
  dplyr::filter(year == 2020) |>
  dplyr::select(slug, ntl_mean_2020 = ntl_mean)
ntl_stats_2025 <- ntl_ts_data |>
  dplyr::filter(year == 2025) |>
  dplyr::select(slug, ntl_mean_2025 = ntl_mean)

city_index <- city_index |>
  dplyr::left_join(ntl_stats_2000, by = "slug") |>
  dplyr::left_join(ntl_stats_2020, by = "slug") |>
  dplyr::left_join(ntl_stats_2025, by = "slug")

# GHS-POP 1990 & 2025 per city → Rankings columns + per-capita built-up.
pop_wide <- pop_ts_data |>
  dplyr::filter(year %in% c(1990, 2025)) |>
  dplyr::select(slug, year, pop) |>
  tidyr::pivot_wider(names_from = year, values_from = pop,
                     names_prefix = "pop_ghs_") |>
  dplyr::mutate(pop_ghs_growth = (pop_ghs_2025 - pop_ghs_1990) / pop_ghs_1990)
city_index <- city_index |> dplyr::left_join(pop_wide, by = "slug")

# Population density = GHS-POP ÷ built-up footprint (persons per km² of built-up
# land, not per km² of the whole polygon). Same denominator as the built-up
# density index above, so the two "density" concepts stay comparable; carries
# the same GHS-POP/GHSL circularity caveat (see About tab).
city_index <- city_index |>
  dplyr::mutate(
    pop_density_1990  = pop_ghs_1990 / footprint_1990_km2,
    pop_density_2025  = pop_ghs_2025 / footprint_2025_km2,
    pop_density_change = pop_density_2025 - pop_density_1990
  )

# Multi-select choices: "City (ISO3)" → slug, so duplicate names disambiguate.
all_city_choices <- setNames(
  city_index$slug,
  paste0(city_index$agglosname, " (", city_index$iso3, ")")
)

# Pre-formatted Rankings tables (built once at startup) — one per theme, chosen
# by the Rankings-tab selector. `rank_sort` names the column each is sorted by
# (descending) and that carries the value bar.
rank_tables <- list(
  `Built-up` = city_index |>
    dplyr::transmute(
      City                  = agglosname,
      Country               = country,
      Region                = as.character(macro_region),
      `Built-up 1990 (km²)` = round(total_km2_1990, 1),
      `Built-up 2025 (km²)` = round(total_km2_2025, 1),
      `Change (km²)`        = round(delta_total_km2_1990_2025, 1),
      `Growth (%)`          = round(pct_growth * 100, 1),
      `Sprawl (%)`          = round(sprawl_share * 100, 1),
      `Density change`      = round(density_change, 3),
      `Tree cover (%)`      = round(p_tree_cov, 1),
      `Growth timing`       = growth_timing,
      `Growth form`         = growth_form,
      `Boundary quality`    = boundary_quality
    ) |>
    dplyr::arrange(dplyr::desc(`Growth (%)`)),

  `Nighttime lights` = city_index |>
    dplyr::transmute(
      City                    = agglosname,
      Country                 = country,
      Region                  = as.character(macro_region),
      `Mean NTL 2000 (DN)`    = round(ntl_mean_2000, 1),
      `Mean NTL 2025 (DN)`    = round(ntl_mean_2025, 1),
      `NTL change (DN)`       = round(ntl_mean_2025 - ntl_mean_2000, 1),
      `Dark/dim 2000 (%)`     = round(darkdim_share_2000 * 100, 1),
      `Dark/dim 2025 (%)`     = round(darkdim_share_2025 * 100, 1),
      `Dark/dim change (pp)`  = round((darkdim_share_2025 - darkdim_share_2000) * 100, 1)
    ) |>
    dplyr::arrange(dplyr::desc(`NTL change (DN)`)),

  `Population` = city_index |>
    dplyr::transmute(
      City                     = agglosname,
      Country                  = country,
      Region                   = as.character(macro_region),
      `Pop 1990 (M)`           = round(pop_ghs_1990 / 1e6, 2),
      `Pop 2025 (M)`           = round(pop_ghs_2025 / 1e6, 2),
      `Pop growth (%)`         = round(pop_ghs_growth * 100, 1),
      `Built-up/cap 1990 (m²)` = round(total_km2_1990 * 1e6 / pop_ghs_1990, 1),
      `Built-up/cap 2025 (m²)` = round(total_km2_2025 * 1e6 / pop_ghs_2025, 1),
      `Pop density 1990 (per km²)` = round(pop_density_1990, 0),
      `Pop density 2025 (per km²)` = round(pop_density_2025, 0),
      `Density change (per km²)`   = round(pop_density_change, 0)
    ) |>
    dplyr::arrange(dplyr::desc(`Pop growth (%)`))
)
rank_sort <- c(`Built-up` = "Growth (%)",
               `Nighttime lights` = "NTL change (DN)",
               `Population` = "Pop growth (%)")

# Slug picker for the quick-filter preset buttons.
preset_slugs <- function(kind, n = 5) {
  ci <- city_index
  picked <- switch(kind,
    fast  = dplyr::slice_max(ci, pct_growth,      n = n),
    slow  = dplyr::slice_min(ci, pct_growth,      n = n),
    large = dplyr::slice_max(ci, total_km2_2025,  n = n),
    small = dplyr::slice_min(ci, total_km2_2025,  n = n)
  )
  picked$slug
}

# Change values are heavily right-skewed (median 372, p99 ~4400 m²/pixel) and
# effectively one-sided (negatives = 0.03% of pixels, treated as NA). Display
# on a sqrt scale with viridis; zeros render transparent so the basemap shows.
epochs      <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025)
UPPER       <- 4400                       # cap = ~p99 of positive change
legend_brks <- c(1, 100, 500, 1000, 2000, UPPER)   # start at 1: zeros are transparent

# Absolute built-up (single-year mode): GHSL values are 0–10,000 m²/pixel
UPPER_ABS        <- 10000
legend_brks_abs  <- c(0, 1000, 2500, 5000, 7500, UPPER_ABS)

pal_res      <- colorNumeric("YlOrRd",  domain = c(0, sqrt(UPPER)),     na.color = "transparent")
pal_nres     <- colorNumeric("viridis", domain = c(0, sqrt(UPPER)),     na.color = "transparent")
pal_res_abs  <- colorNumeric("YlOrRd",  domain = c(0, sqrt(UPPER_ABS)), na.color = "transparent")
pal_nres_abs <- colorNumeric("viridis", domain = c(0, sqrt(UPPER_ABS)), na.color = "transparent")
pal_tree     <- colorNumeric("Greens",  domain = c(0, 100),             na.color = "#cccccc")

# NTL & lit/unlit palettes — bloom/top-code-corrected DMSP, corrected-DN scale
# (NOT radiance). Per-pixel DN for a large agglomeration in 2025 runs roughly
# median ~30, p90 ~75, p99 ~120. Breaks step around the pipeline's dim/bright
# split at DN 10 (see scripts/1_create_citydata.R).
NTL_UPPER    <- 200
NTL_BREAKS   <- c(0, 10, 30, 60, 100, NTL_UPPER)
NTL_LABELS   <- c("0 (unlit)", "10 (dim ceiling)", "30", "60", "100", "200+")
# log1p transform spreads low-value pixels across the palette (most African city
# pixels sit under ~DN 60; a linear 0–200 scale would render them all near-black).
# The legend shows original-scale labels; log1p is applied to the break values
# when requesting colours from the palette.
pal_ntl      <- colorNumeric("inferno", domain = c(0, log1p(NTL_UPPER)), na.color = "transparent")
# lit_unlit rasters are 4-state: 0 unbuilt / 1 unlit (DN 0) / 2 dim (DN 1–10) /
# 3 bright (DN > 10).
LU_LEVELS    <- c(1, 2, 3)
LU_COLS      <- c("#b30000", "#fd8d3c", "#ffffb2")   # unlit / dim / bright
LU_LABELS    <- c("Built — unlit (DN 0)", "Built — dim (DN 1–10)", "Built — bright (DN > 10)")
pal_lu       <- colorFactor(LU_COLS, levels = LU_LEVELS, na.color = "transparent")

sqrt_capped <- function(r, upper = UPPER) {
  if (inherits(r, "SpatRaster")) r <- raster::raster(r)
  v <- raster::values(r)
  v[v <= 0] <- NA
  v <- sqrt(pmin(v, upper))
  raster::setValues(r, v)
}

# Info panel for the Urban Growth tab — dynamic on the selected year range.
# `mode` is "single" or "change". In "change" mode the built-up, population and
# sprawl/density rows all refer to y0 -> y1; in "single" mode only y0 is used.
# `gs` is the growth_stats() list for (y0, y1) (sprawl / footprint / density);
# pass NULL to omit those rows.
city_info_html <- function(name, mode, y0, y1,
                           bu_y0, bu_y1, pop_y0, pop_y1,
                           tree_cov, gs = NULL,
                           boundary_flag = NA_character_,
                           growth_timing = NA_character_,
                           growth_form   = NA_character_) {
  km  <- function(x) if (is.na(x)) "N/A" else sprintf("%.1f km&sup2;", x)
  ppl <- function(x) if (is.na(x)) "N/A" else format(round(x), big.mark = ",", scientific = FALSE)
  gpct <- function(x) if (is.na(x)) "N/A" else sprintf("%+.0f%%", x * 100)
  tree_str <- if (is.na(tree_cov)) "N/A" else sprintf("%.1f%%", tree_cov)

  row <- function(label, value, top = FALSE) {
    paste0("<tr", if (top) " style='border-top:1px solid #ddd'" else "", ">",
           "<td style='color:#555;padding-right:8px'>", label, "</td>",
           "<td style='text-align:right'>", value, "</td></tr>")
  }

  pdens <- function(pop, fp) if (is.na(pop) || is.na(fp) || fp == 0) NA_real_ else pop / fp
  ppkm  <- function(x) if (is.na(x)) "N/A" else paste0(format(round(x), big.mark = ",", scientific = FALSE), "/km&sup2;")

  if (mode == "single") {
    pd0 <- if (!is.null(gs)) pdens(pop_y0, gs$footprint_0) else NA_real_
    body <- paste0(
      row(paste0("Built-up ", y0), km(bu_y0)),
      if (!is.null(gs)) row(paste0("Footprint ", y0), km(gs$footprint_0)) else "",
      if (!is.null(gs) && !is.na(gs$density_0))
        row(paste0("Density index ", y0), sprintf("%.3f", gs$density_0)) else "",
      row(paste0("Population ", y0), ppl(pop_y0), top = TRUE),
      if (!is.na(pd0)) row(paste0("Pop density ", y0), ppkm(pd0)) else "",
      row("Tree cover (2020)", tree_str, top = TRUE)
    )
    footnote <- ""
  } else {
    d_bu  <- if (is.na(bu_y0)  || is.na(bu_y1))  NA_real_ else bu_y1 - bu_y0
    g_bu  <- if (is.na(d_bu)   || is.na(bu_y0)  || bu_y0  == 0) NA_real_ else d_bu / bu_y0
    d_pop <- if (is.na(pop_y0) || is.na(pop_y1)) NA_real_ else pop_y1 - pop_y0
    g_pop <- if (is.na(d_pop)  || is.na(pop_y0) || pop_y0 == 0) NA_real_ else d_pop / pop_y0
    pd0   <- if (!is.null(gs)) pdens(pop_y0, gs$footprint_0) else NA_real_
    pd1   <- if (!is.null(gs)) pdens(pop_y1, gs$footprint_1) else NA_real_

    sprawl_rows <- if (!is.null(gs) && !is.na(gs$sprawl_share)) paste0(
      row("New land (sprawl)",
          sprintf("%.1f km&sup2; (%.0f%%)", gs$sprawl_km2, gs$sprawl_share * 100), top = TRUE),
      row("Densification",
          sprintf("%.1f km&sup2; (%.0f%%)", gs$intens_km2, (1 - gs$sprawl_share) * 100)),
      row("Density trend*",
          if (is.na(gs$density_change)) "N/A"
          else if (gs$density_change > 0) sprintf("+%.3f (densifying)", gs$density_change)
          else sprintf("%.3f (sprawling)", gs$density_change))
    ) else ""

    body <- paste0(
      row(paste0("Built-up ", y0), km(bu_y0)),
      row(paste0("Built-up ", y1), km(bu_y1)),
      row(sprintf("Increase %s&ndash;%s", y0, y1),
          sprintf("<b>%s (%s)</b>",
                  if (is.na(d_bu)) "N/A" else sprintf("+%.1f km&sup2;", d_bu), gpct(g_bu)),
          top = TRUE),
      row(paste0("Population ", y0), ppl(pop_y0), top = TRUE),
      row(paste0("Population ", y1), ppl(pop_y1)),
      row(sprintf("Pop change %s&ndash;%s", y0, y1),
          sprintf("<b>%s (%s)</b>",
                  if (is.na(d_pop)) "N/A" else paste0("+", ppl(d_pop)), gpct(g_pop))),
      if (!is.na(pd0) || !is.na(pd1)) paste0(
        row(paste0("Pop density ", y0), ppkm(pd0)),
        row(paste0("Pop density ", y1), ppkm(pd1))
      ) else "",
      sprawl_rows,
      row("Tree cover (2020)", tree_str, top = TRUE)
    )
    footnote <- if (!is.null(gs) && !is.na(gs$sprawl_share)) paste0(
      "<p style='margin:6px 0 0;font-size:10px;color:#888;",
      "border-top:1px solid #eee;padding-top:4px'>",
      "* Density trend = built-up surface &divide; built-up footprint (both km&sup2;),<br>",
      y1, " minus ", y0, ".<br>Positive = densifying; negative = footprint spreading thin.",
      "</p>"
    ) else ""
  }

  warning_html <- if (isTRUE(boundary_flag == "Boundary-saturated")) paste0(
    "<div style='margin:8px 12px 0;padding:6px 8px;font-size:11px;",
    "background:#fff3cd;border:1px solid #ffe69c;border-radius:4px;color:#664d03'>",
    "&#9888; Boundary-saturated: built-up runs right to this polygon&rsquo;s edge &mdash; ",
    "growth here may be undercounted by the fixed boundary rather than slowing for real.",
    "</div>"
  ) else ""

  tags_html <- if (!is.na(growth_timing) || !is.na(growth_form)) paste0(
    "<div style='padding:6px 12px 0;font-size:10px;color:#666'>",
    "1990&ndash;2025 profile: ",
    "<b>", if (is.na(growth_timing)) "N/A" else growth_timing, "</b> &middot; ",
    "<b>", if (is.na(growth_form)) "N/A" else growth_form, "</b>",
    "</div>"
  ) else ""

  HTML(paste0(
    "<div style='background:rgba(255,255,255,0.95);border-radius:6px;",
    "box-shadow:0 1px 5px rgba(0,0,0,0.4);min-width:230px;max-width:290px;",
    "font-size:12px;font-family:sans-serif'>",
    "<div onclick=\"var b=this.nextElementSibling;var a=this.querySelector('.arr');",
    "if(b.style.display==='none'){b.style.display='block';a.innerHTML='&#9660;'}",
    "else{b.style.display='none';a.innerHTML='&#9654;'}\" ",
    "style='padding:8px 12px;cursor:pointer;font-weight:600;background:#e8e8e8;",
    "border-radius:6px 6px 0 0;display:flex;justify-content:space-between;align-items:center'>",
    "<span>", htmltools::htmlEscape(name), "</span>",
    "<span class='arr'>&#9660;</span></div>",
    warning_html,
    tags_html,
    "<div style='padding:8px 12px'>",
    "<table style='width:100%;border-collapse:collapse;line-height:1.6'>",
    body,
    "</table>",
    footnote,
    "<p style='margin:6px 0 0;font-size:10px;color:#888;border-top:1px solid #eee;padding-top:4px'>",
    "Population: GHS-POP R2023A &mdash; disaggregated using built-up,<br>so per-capita figures are partly circular.",
    "</p>",
    "</div></div>"
  ))
}

# Info panel for the Nighttime Lights tab.
# Shows mean NTL intensity (corrected DN, 2000 / 2025 / change) and the
# dark-or-dim built-up share (DN <= 10; 2000 / 2025 / change).
ntl_info_html <- function(name, ntl_2000, ntl_2025,
                           darkdim_2000, darkdim_2025) {
  fmt_ntl <- function(x) if (is.na(x)) "N/A" else sprintf("%.1f", x)
  ntl_delta <- if (is.na(ntl_2000) || is.na(ntl_2025)) NA_real_
               else ntl_2025 - ntl_2000
  ntl_delta_str <- if (is.na(ntl_delta)) "N/A"
                   else sprintf("%+.1f", ntl_delta)

  fmt_pct <- function(x) if (is.na(x)) "N/A" else sprintf("%.1f%%", x * 100)
  dd_delta_pp <- if (is.na(darkdim_2000) || is.na(darkdim_2025)) NA_real_
                    else (darkdim_2025 - darkdim_2000) * 100
  dd_delta_str <- if (is.na(dd_delta_pp)) "N/A"
                     else sprintf("%+.1f pp", dd_delta_pp)

  HTML(paste0(
    "<div style='background:rgba(255,255,255,0.95);border-radius:6px;",
    "box-shadow:0 1px 5px rgba(0,0,0,0.4);min-width:240px;",
    "font-size:12px;font-family:sans-serif'>",
    "<div onclick=\"var b=this.nextElementSibling;var a=this.querySelector('.arr');",
    "if(b.style.display==='none'){b.style.display='block';a.innerHTML='&#9660;'}",
    "else{b.style.display='none';a.innerHTML='&#9654;'}\" ",
    "style='padding:8px 12px;cursor:pointer;font-weight:600;background:#e8e8e8;",
    "border-radius:6px 6px 0 0;display:flex;justify-content:space-between;align-items:center'>",
    "<span>", htmltools::htmlEscape(name), "</span>",
    "<span class='arr'>&#9660;</span></div>",
    "<div style='padding:8px 12px'>",
    "<table style='width:100%;border-collapse:collapse;line-height:1.6'>",
    "<tr><td colspan='2' style='color:#444;font-weight:600;padding-bottom:2px'>",
    "Mean NTL (corrected DN)</td></tr>",
    "<tr><td style='color:#555;padding-right:8px'>2000</td>",
    "<td style='text-align:right'>", fmt_ntl(ntl_2000), "</td></tr>",
    "<tr><td style='color:#555;padding-right:8px'>2025</td>",
    "<td style='text-align:right'>", fmt_ntl(ntl_2025), "</td></tr>",
    "<tr style='border-bottom:1px solid #ddd'>",
    "<td style='color:#555;padding-right:8px'>Change</td>",
    "<td style='text-align:right;font-weight:600'>", ntl_delta_str, "</td></tr>",
    "<tr><td colspan='2' style='color:#444;font-weight:600;",
    "padding-top:6px;padding-bottom:2px'>Dark or dim built-up share</td></tr>",
    "<tr><td style='color:#555;padding-right:8px'>2000</td>",
    "<td style='text-align:right'>", fmt_pct(darkdim_2000), "</td></tr>",
    "<tr><td style='color:#555;padding-right:8px'>2025</td>",
    "<td style='text-align:right'>", fmt_pct(darkdim_2025), "</td></tr>",
    "<tr><td style='color:#555;padding-right:8px'>Change</td>",
    "<td style='text-align:right;font-weight:600'>", dd_delta_str, "</td></tr>",
    "</table>",
    "<p style='margin:6px 0 0;font-size:10px;color:#888;border-top:1px solid #eee;padding-top:4px'>",
    "Corrected DN: bloom- & top-code-corrected DMSP. Dark = DN 0, dim = DN 1&ndash;10 ",
    "(below the DMSP detection floor) &mdash; a proxy for informal / low-density built-up.",
    "</p>",
    "</div></div>"
  ))
}

load_assets <- function(slug) {
  if (is.null(cities_root)) return(NULL)
  d <- file.path(cities_root, slug)
  if (!dir.exists(d)) return(NULL)
  tot_f <- file.path(d, "total.tif")   # native Mollweide, equal-area — for stats
  list(
    metro        = sf::st_read(file.path(d, "metro.gpkg"), quiet = TRUE),
    res_stack    = terra::rast(file.path(d, "res_wgs84.tif")),
    nres_stack   = terra::rast(file.path(d, "nres_wgs84.tif")),
    total_native = if (file.exists(tot_f)) terra::rast(tot_f) else NULL
  )
}

# Sprawl / footprint / density decomposition for a city over (y0, y1),
# recomputed at runtime so the Urban Growth info box tracks the year picker.
# Mirrors scripts/2_4_sprawl_metrics.R exactly (native Mollweide, 100 m pixels =
# 0.01 km²): at y0 == y1 the change terms are 0 and only the y0 stock/footprint/
# density fields are meaningful.
growth_stats <- function(a, y0, y1) {
  r <- a$total_native
  if (is.null(r)) return(NULL)
  yn <- names(r)
  if (!(as.character(y0) %in% yn) || !(as.character(y1) %in% yn)) return(NULL)
  PIXEL_KM2 <- 0.01
  r0 <- r[[as.character(y0)]]
  r1 <- r[[as.character(y1)]]
  s  <- function(x) sum(terra::values(x), na.rm = TRUE)
  bu0 <- s(r0) / 1e6
  bu1 <- s(r1) / 1e6
  fp0 <- sum(terra::values(r0) > 0, na.rm = TRUE) * PIXEL_KM2
  fp1 <- sum(terra::values(r1) > 0, na.rm = TRUE) * PIXEL_KM2
  sprawl <- s(terra::ifel(r0 == 0 & r1 > 0,  r1,      0)) / 1e6
  intens <- s(terra::ifel(r0 > 0  & r1 > r0, r1 - r0, 0)) / 1e6
  chg    <- sprawl + intens
  list(
    bu0 = bu0, bu1 = bu1,
    footprint_0 = fp0, footprint_1 = fp1,
    sprawl_km2 = sprawl, intens_km2 = intens,
    sprawl_share   = if (chg > 0) sprawl / chg else NA_real_,
    density_0      = if (fp0 > 0) bu0 / fp0 else NA_real_,
    density_1      = if (fp1 > 0) bu1 / fp1 else NA_real_,
    density_change = if (fp0 > 0 && fp1 > 0) bu1 / fp1 - bu0 / fp0 else NA_real_
  )
}

load_ntl_assets <- function(slug) {
  if (is.null(cities_root)) return(NULL)
  d <- file.path(cities_root, slug)
  if (!dir.exists(d)) return(NULL)
  list(
    metro     = sf::st_read(file.path(d, "metro.gpkg"), quiet = TRUE),
    ntl_stack = terra::rast(file.path(d, "ntl_wgs84.tif")),
    lu_stack  = terra::rast(file.path(d, "lit_unlit_wgs84.tif"))
  )
}

map_sidebar <- sidebar(
  width = 300,
  selectInput("country", "Country",
              choices  = country_choices,
              selected = default_country),
  selectInput("city", "City",
              choices  = cities_for(default_country),
              selected = default_city),
  hr(),
  radioButtons("map_mode", "Display mode",
               choices  = c("Single year" = "single", "Period change" = "change"),
               selected = "change",
               inline   = TRUE),
  conditionalPanel(
    condition = "input.map_mode == 'single'",
    selectInput("yr_single", "Year",
                choices  = epochs,
                selected = 2025)
  ),
  conditionalPanel(
    condition = "input.map_mode == 'change'",
    div(class = "d-flex gap-2",
      selectInput("yr_start", "From",
                  choices  = epochs[-length(epochs)],
                  selected = 1990,
                  width    = "50%"),
      selectInput("yr_end", "To",
                  choices  = epochs[-1],
                  selected = 2025,
                  width    = "50%")
    ),
    hr(),
    radioButtons("margin", "Margin type",
                 choices  = c("Intensive (change in existing built-up)" = "intensive",
                              "Extensive (new land)"                    = "extensive"),
                 selected = "intensive")
  )
)

ts_sidebar <- sidebar(
  width = 320,
  selectizeInput(
    "cities_ts", "Cities",
    choices  = all_city_choices,
    selected = default_city,
    multiple = TRUE,
    options  = list(plugins = list("remove_button"))
  ),
  helpText("Add a city:"),
  selectInput("ts_country", "Country",
              choices  = country_choices,
              selected = default_country),
  selectInput("ts_city", "City",
              choices  = cities_for(default_country)),
  actionButton("ts_add_city", "Add to comparison",
               class = "btn-sm btn-outline-secondary w-100"),
  hr(),
  radioButtons("ts_builtup_type", "Built-up type",
               choices  = c("Total" = "total", "Residential" = "res", "Non-residential" = "nres"),
               selected = "total",
               inline   = TRUE),
  radioButtons("ts_scale", "Scale (built-up panel)",
               choices = c(
                 "Absolute"               = "abs",
                 "Growth rate (2000=100)" = "idx",
                 "Per 1,000 residents"    = "pop"
               ),
               selected = "idx"),
  helpText("Population (GHS-POP), mean NTL and dark/dim share are always shown in absolute units.")
)

ntl_sidebar <- sidebar(
  width = 300,
  selectInput("ntl_country", "Country",
              choices  = country_choices,
              selected = default_country),
  selectInput("ntl_city", "City",
              choices  = cities_for(default_country)),
  hr(),
  radioButtons("ntl_layer", "Layer",
               choices  = c("NTL intensity" = "ntl", "Lit / unlit built-up" = "lu"),
               selected = "ntl"),
  sliderInput("ntl_epoch", "Year",
              min = 2000, max = 2025, value = 2025, step = 5,
              sep = "", ticks = TRUE,
              animate = animationOptions(interval = 3500, loop = TRUE)),
  helpText("Press play to step through 2000, 2005, …, 2025.")
)

ui <- page_navbar(
  title = "Urban Africa",
  header = tags$style(HTML(
    "@media (max-width: 767px) {
      #map      { height: 60vh !important; min-height: 300px !important; }
      #ntl_map  { height: 60vh !important; min-height: 300px !important; }
    }
    /* Ensure flag emoji render as images rather than letter-pairs on Windows 10 */
    .selectize-input, .selectize-input *,
    .selectize-dropdown, .selectize-dropdown * {
      font-family: 'Twemoji Mozilla', 'Apple Color Emoji', 'Segoe UI Emoji',
                   'Noto Color Emoji', 'EmojiOne Color', sans-serif !important;
    }"
  )),
  nav_panel(
    "Urban Growth",
    layout_sidebar(
      sidebar = map_sidebar,
      shinycssloaders::withSpinner(
        leafletOutput("map", height = "85vh"),
        type = 6, color = "#555555"
      )
    )
  ),
  nav_panel(
    "Nighttime Lights",
    layout_sidebar(
      sidebar = ntl_sidebar,
      shinycssloaders::withSpinner(
        leafletOutput("ntl_map", height = "85vh"),
        type = 6, color = "#555555"
      )
    )
  ),
  nav_panel(
    "Time series",
    layout_sidebar(
      sidebar = ts_sidebar,
      div(
        style = "overflow-y: auto; height: 85vh;",
        plotOutput("ts_plot", height = "820px")
      )
    )
  ),
  nav_panel(
    "Rankings",
    div(
      style = "padding: 12px",
      div(
        style = "margin-bottom: 10px; display: flex; justify-content: space-between; align-items: center; gap: 12px; flex-wrap: wrap",
        h5("Rankings — 100 largest African agglomerations", style = "margin: 0"),
        div(
          style = "display: flex; align-items: center; gap: 12px",
          radioButtons("rank_view", NULL,
                       choices  = names(rank_tables),
                       selected = "Built-up",
                       inline   = TRUE),
          downloadButton("dl_table", "Download CSV", class = "btn-sm btn-outline-secondary")
        )
      ),
      DT::dataTableOutput("league_table")
    )
  ),
  nav_panel(
    "Scatterplots",
    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        radioButtons(
          "scatter_type", "Comparison",
          choices = c(
            "Initial extent vs. growth"        = "extent_growth",
            "Sprawl vs. intensification"       = "sprawl_intens",
            "Population growth vs. land growth" = "pop_vs_builtup"
          ),
          selected = "extent_growth"
        )
      ),
      plotly::plotlyOutput("scatter", height = "85vh")
    )
  ),
  nav_panel(
    "About",
    div(
      style = "max-width: 820px; margin: 40px auto; padding: 0 20px 60px",

      h3("Urban Africa — methodology & data sources"),
      p(
        "This app tracks urban built-up expansion across the ",
        tags$b("100 largest African agglomerations"), " (by 2020 population) from 1990 to 2025.",
        " It combines satellite-derived built-up surface data with nighttime light (NTL) composites",
        " to characterise both the ", em("extent"), " and the ", em("form"), " of urban growth —",
        " distinguishing sprawl from densification and lit (formal) from unlit (informal) built-up."
      ),

      hr(),
      h4("App guide"),
      tags$dl(
        tags$dt(tags$b("Urban Growth")),
        tags$dd(
          "Interactive map of residential and non-residential built-up surface.",
          " Choose between a single-year snapshot or the change between any two epochs.",
          " Change can be shown as the ", tags$b("intensive margin"), " (densification within",
          " the existing footprint) or the ", tags$b("extensive margin"), " (greenfield expansion).",
          " The info panel is dynamic: built-up stock, increase, population (GHS-POP), sprawl",
          " decomposition and density trend all follow the selected From→To years; tree cover is 2020.",
          " A 1990–2025 growth-timing / growth-form tag and a boundary-saturation warning",
          " (see Known caveats below) also appear where applicable."
        ),
        tags$dt(tags$b("Nighttime Lights")),
        tags$dd(
          "NTL intensity raster (log-scaled, capped at DN 200 on the corrected-DN scale) or a",
          " pixel-level unlit / dim / bright classification of built-up for any epoch 2000–2025.",
          " The info panel shows city-level mean NTL and dark-or-dim built-up shares for 2000 and 2025."
        ),
        tags$dt(tags$b("Time Series")),
        tags$dd(
          "A 2×2 grid of multi-city line charts: built-up area (total, residential, or",
          " non-residential), population (GHS-POP), mean NTL (corrected DN), and dark-or-dim",
          " built-up share. Built-up can be shown in absolute km², as a growth index (1990 = 100),",
          " or per 1,000 residents (time-varying GHS-POP denominator).",
          " Use the country / city picker and ", tags$em("Add to comparison"), " button to build",
          " a custom comparison set."
        ),
        tags$dt(tags$b("Rankings")),
        tags$dd(
          "Sortable table of all 100 agglomerations with built-up extent (1990 & 2025),",
          " absolute and percentage growth, sprawl share, density change, GHS-POP population and",
          " growth, built-up per capita, tree cover, and dark-or-dim built-up share.",
          " The Built-up view also carries a ", tags$b("growth timing"), " tag (Accelerating /",
          " Steady / Decelerating, comparing 1990–2005 vs. 2010–2025 growth rates), a ",
          tags$b("growth form"), " tag (Sprawl-dominant / Mixed / Infill-dominant, by sprawl",
          " share) and a ", tags$b("boundary quality"), " flag (see Known caveats below).",
          " Filterable and downloadable as CSV."
        ),
        tags$dt(tags$b("Scatterplots")),
        tags$dd(
          tags$em("Initial extent vs. growth:"), " identifies whether larger cities grew more in absolute terms.",
          tags$br(),
          tags$em("Sprawl vs. intensification:"), " plots new-land growth against within-footprint",
          " densification; cities above the diagonal are sprawl-dominant.",
          tags$br(),
          tags$em("Population growth vs. land growth:"), " plots 1990–2025 population growth against",
          " built-up growth; cities above the diagonal grew their footprint faster than their",
          " population (thinning out), cities below it densified."
        )
      ),

      hr(),
      h4("Data sources"),
      tags$ul(
        tags$li(
          tags$b("GHSL-BUILT-S R2023 (built-up surface):"),
          " Global Human Settlement Layer, European Commission Joint Research Centre.",
          " Built-up surface area in m² per 100 m × 100 m pixel, provided in equal-area",
          " Mollweide projection (EPSG:54009).",
          " Eight epochs: 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025.",
          " Separate layers for total, residential, and non-residential built-up.",
          " The 2025 layer is model-extrapolated, not directly observed."
        ),
        tags$li(
          tags$b("Africapolis 2020 (agglomeration boundaries):"),
          " Urban agglomeration polygons, 2020 population estimates, and tree-cover percentage",
          " for African agglomerations (OECD/Sahel and West Africa Club, SWAC).",
          " Provides the spatial units used to aggregate all raster statistics.",
          " Polygon geometry for the top-100 set is repaired (ring-winding and degenerate-vertex",
          " fixes) before extraction. Two agglomerations are excluded and replaced by the",
          " next-ranked: ", tags$b("Kisumu"), " (Africapolis defines it as a ~15.5-million-person,",
          " ~21,000 km² Lake-Victoria settlement continuum, not a coherent metro) and ",
          tags$b("Port Harcourt"), " (Niger-Delta gas-flare masking zeroes its corrected",
          " nighttime-light signal in every year)."
        ),
        tags$li(
          tags$b("Bloom- and top-coding-corrected DMSP nighttime lights"),
          " (Chiovelli, Michalopoulos, Papaioannou & Regan, 2026 — ",
          tags$em("Illuminating the Global South"), "):",
          " annual DMSP-OLS composites, 1992–2025, ~1 km, corrected for the two principal DMSP",
          " artefacts — ", tags$em("blooming"), " (light spilling beyond its physical source) and ",
          tags$em("top-coding"), " (bright urban cores saturating at the sensor's digital-number",
          " ceiling of 63). Values are on a corrected / extended digital-number (DN) scale, not",
          " radiance. This single corrected series replaces the earlier DMSP-OLS + VIIRS blend and",
          " needs no cross-sensor harmonisation; 2025 is native (no proxy year)."
        ),
        tags$li(
          tags$b("GHSL GHS-POP R2023A (residential population):"),
          " Modelled population per 100 m pixel, sharing the grid, Mollweide CRS (EPSG:54009) and",
          " 5-year epochs (1990–2025) of the built-up layer, summed within each agglomeration",
          " polygon. Drives the population time-series and the per-capita built-up figures.",
          tags$em(" Caveat:"), " GHS-POP is spatially disaggregated ", tags$em("using"),
          " the GHSL built-up layer, so any per-capita built-up density or population density",
          " derived from the two is partly circular."
        )
      ),

      hr(),
      h4("Key metrics"),

      tags$h5("Built-up surface area"),
      p(
        "GHSL pixel values (m² of built-up surface per 100 m pixel, range 0–10,000) are summed",
        " across all pixels within each agglomeration polygon and divided by 1,000,000 to yield km².",
        " Processing uses the native equal-area Mollweide CRS throughout; pixels are reprojected",
        " to WGS 84 only for map display."
      ),

      tags$h5("Margins of urban expansion"),
      tags$ul(
        tags$li(
          tags$b("Intensive margin (densification):"),
          " Pixels that had non-zero built-up surface in the base year and",
          " increased by the end year. Measures infilling and vertical growth within the existing footprint."
        ),
        tags$li(
          tags$b("Extensive margin (sprawl / new land):"),
          " Pixels with zero built-up surface in the base year that became positive by the end year.",
          " Measures greenfield expansion onto previously undeveloped land."
        )
      ),

      tags$h5("Sprawl decomposition"),
      p(
        "Total growth is decomposed into sprawl (extensive) and densification (intensive) km².",
        " The ", tags$b("sprawl share"), " is the fraction of net new built-up surface that",
        " came from new pixels rather than intensification of existing ones.",
        " A shrinkage component (pixels where surface declined) is tracked separately but",
        " is negligible for most cities.",
        " The Rankings table reports the full 1990–2025 decomposition; the Urban Growth info",
        " box recomputes it for whichever From→To years are selected."
      ),

      tags$h5("Density trend"),
      p(
        "The ", tags$b("density index"), " is defined as total built-up surface (km²) divided by",
        " the built-up footprint (km², the count of pixels with any built-up surface × 0.01 km²/pixel).",
        " A rising density index means built-up surface is growing faster than the footprint —",
        " the city is filling in. A falling index means the footprint is expanding faster than",
        " the surface — built form is spreading thin.",
        " The ", tags$b("density change"), " is the end-year index minus the start-year index",
        " (1990–2025 in Rankings, the selected range in the info box);",
        " positive = densifying, negative = sprawling."
      ),

      tags$h5("Population density"),
      p(
        "Reported as GHS-POP population divided by the ", tags$b("built-up footprint"),
        " (km², the same denominator as the density index above) rather than by the whole",
        " polygon area, so it reads as persons per km² of built-up land, not per km² of the",
        " (often mostly-empty) agglomeration boundary.",
        " Shown in the Urban Growth info box for whichever From→To years are selected, and in",
        " the Population Rankings table for 1990 and 2025.",
        tags$em(" Caveat:"), " inherits the same GHS-POP/GHSL circularity as per-capita",
        " built-up (see Data sources above)."
      ),

      tags$h5("Nighttime light classification"),
      p(
        "Each built-up pixel is classed by its corrected DN in the matching year: ",
        tags$b("unlit"), " (DN = 0), ", tags$b("dim"), " (DN 1–10, the DMSP “marginal light”",
        " range) or ", tags$b("bright"), " (DN > 10).",
        " Because the blooming correction removes the dim halo that a radiance threshold used to",
        " filter out, a bare lit / unlit split at DN > 0 saturates for large agglomerations by",
        " 2025. The app therefore reports a ", tags$b("dark-or-dim share"), " (DN ≤ 10 as a",
        " fraction of built-up), which retains a meaningful, declining trend — roughly 44% → 14%",
        " of built-up across the top 100 between 2000 and 2025.",
        " Unlit / dim built-up is a proxy for informal or low-density settlement below the DMSP",
        " detection floor.",
        " NTL maps use a log₁p scale (DN capped at 200) to spread the low-value pixels that",
        " dominate African cities."
      ),

      p(
        tags$em(
          "Note: the corrected DMSP series is annual and native through 2025, so no proxy year",
          " is used. Early-1990s composites are noisier than later years; the time-series panels",
          " start at 2000 for comparability with the built-up series."
        ),
        style = "font-size:12px; color:#666; border-left:3px solid #ddd; padding-left:10px; margin-top:4px"
      ),

      hr(),
      h4("Known caveats"),
      tags$ul(
        tags$li(
          tags$b("Abuja's growth figure is likely an undercount."),
          " A boundary-saturation check (edge-vs-interior built-up fill within each Africapolis",
          " polygon) found Abuja's polygon nearly wall-to-wall built-up right up to its border —",
          " on top of already having the single largest growth figure in the sample",
          " (+404%, 1990–2025). Its apparent post-2010 growth slowdown may partly be a",
          " measurement ceiling — the fixed 2020-vintage boundary capping how much further",
          " growth can still register — rather than a genuine deceleration.",
          " Ten other cities show the same pattern more mildly (Agadir, Johannesburg, Durban,",
          " Constantine, Cape Town, Onitsha, Nsukka, Al-Iskandariya, Bafoussam, Harare).",
          " Every other megacity (Lagos, Kinshasa, Nairobi, Kigali, Abidjan, Accra) shows no",
          " such signal — their growth numbers aren't boundary-capped."
        ),
        tags$li(
          tags$b("Cities are sprawling out, not building up."),
          " A one-off check (GHS-BUILT-V building volume ÷ BUILT-S footprint = mean building",
          " height, 2000 vs. 2025 only) found the median height across the top 100 ",
          tags$em("fell"), " from 7.67 m to 7.25 m — footprint growth (median +48%)",
          " consistently outran volume growth (median +36%). Most of the measured expansion is",
          " horizontal, low-rise growth at the edge rather than vertical growth within existing",
          " footprints, even where individual downtown cores may be getting taller."
        )
      ),

      hr(),
      h4("Units & display scales"),
      tags$ul(
        tags$li("All area figures: ", tags$b("km²")),
        tags$li("NTL intensity: ", tags$b("corrected DN"),
                " (bloom- and top-coding-corrected DMSP digital number; not radiance)"),
        tags$li(
          "Built-up change maps: ", tags$b("sqrt scale"),
          " — the raw change distribution is heavily right-skewed (most pixels show modest change;",
          " a small fraction show very large values). Square-root scaling compresses outliers",
          " and reveals spatial patterns across the full range. Zero-change pixels are transparent."
        ),
        tags$li(
          "NTL intensity maps: ", tags$b("log₁p scale"),
          " — most urban pixels in Africa sit below ~DN 60;",
          " a linear scale would render the majority near-black."
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # --- URL state: restore on startup, update on city change -------------------
  session$onFlushed(function() {
    query <- shiny::parseQueryString(isolate(session$clientData$url_search))
    if (!is.null(query$country) && query$country %in% country_choices) {
      cities <- cities_for(query$country)
      sel    <- if (!is.null(query$city) && query$city %in% cities) query$city else cities[[1]]
      updateSelectInput(session, "country", selected = query$country)
      updateSelectInput(session, "city",    choices = cities, selected = sel)
    }
  }, once = TRUE)

  observe({
    req(input$country, input$city)
    shiny::updateQueryString(
      paste0("?country=", utils::URLencode(input$country, reserved = TRUE),
             "&city=",    input$city),
      mode = "replace"
    )
  })

  observeEvent(input$country, {
    ch  <- cities_for(input$country)
    cur <- isolate(input$city)
    updateSelectInput(session, "city", choices = ch,
                      selected = if (isTRUE(cur %in% ch)) cur else ch[[1]])
  })

  # Cross-tab city sync: NTL tab → Urban Growth tab
  observeEvent(input$ntl_city, {
    req(input$ntl_city)
    if (isTRUE(isolate(input$city) == input$ntl_city)) return()
    row <- city_index[city_index$slug == input$ntl_city, , drop = FALSE]
    if (nrow(row) == 0) return()
    new_country <- row$country[1]
    updateSelectInput(session, "country", selected = new_country)
    updateSelectInput(session, "city",
                      choices  = cities_for(new_country),
                      selected = input$ntl_city)
  }, ignoreInit = TRUE)

  # Cross-tab city sync: Urban Growth tab → NTL tab
  observeEvent(input$city, {
    req(input$city)
    if (isTRUE(isolate(input$ntl_city) == input$city)) return()
    row <- city_index[city_index$slug == input$city, , drop = FALSE]
    if (nrow(row) == 0) return()
    new_country <- row$country[1]
    updateSelectInput(session, "ntl_country", selected = new_country)
    updateSelectInput(session, "ntl_city",
                      choices  = cities_for(new_country),
                      selected = input$city)
  }, ignoreInit = TRUE)

  # Cross-tab carry-over: Urban Growth / NTL → Time Series.
  observeEvent(input$city, {
    req(input$city)
    updateSelectizeInput(session, "cities_ts", selected = input$city)
  }, ignoreInit = TRUE)

  # Keep yr_end choices always after yr_start
  observeEvent(input$yr_start, {
    valid <- epochs[epochs > as.integer(input$yr_start)]
    updateSelectInput(session, "yr_end",
                      choices  = valid,
                      selected = max(valid))
  }, ignoreInit = TRUE)

  assets <- reactive({
    req(input$city)
    load_assets(input$city)
  }) |> bindCache(input$city)

  city_bb <- reactive({
    city_index |> dplyr::filter(slug == input$city)
  })

  output$map <- renderLeaflet({
    a  <- assets(); req(a)
    bb <- city_bb()
    tree_cov <- bb$p_tree_cov

    if (input$map_mode == "single") {
      yr <- as.character(input$yr_single)
      y0 <- y1 <- yr
      r_res      <- a$res_stack[[yr]]
      r_nres     <- a$nres_stack[[yr]]
      title_res  <- sprintf("Residential built-up<br>(m&sup2;/pixel, %s)<br><em>sqrt scale</em>",  yr)
      title_nres <- sprintf("Non-residential built-up<br>(m&sup2;/pixel, %s)<br><em>sqrt scale</em>", yr)
      layer_res  <- "Residential built-up"
      layer_nres <- "Non-residential built-up"
      p_res  <- pal_res_abs
      p_nres <- pal_nres_abs
      upper  <- UPPER_ABS
      brks   <- legend_brks_abs
    } else {
      y0 <- as.character(input$yr_start)
      y1 <- as.character(input$yr_end)
      yr_label <- sprintf("%s&rarr;%s", y0, y1)
      if (input$margin == "extensive") {
        r_res  <- terra::ifel(
          a$res_stack[[y0]]  == 0 & a$res_stack[[y1]]  > 0, a$res_stack[[y1]],  NA)
        r_nres <- terra::ifel(
          a$nres_stack[[y0]] == 0 & a$nres_stack[[y1]] > 0, a$nres_stack[[y1]], NA)
        title_res  <- sprintf("New residential<br>(m&sup2;/pixel, %s)<br><em>sqrt scale</em>",  yr_label)
        title_nres <- sprintf("New non-residential<br>(m&sup2;/pixel, %s)<br><em>sqrt scale</em>", yr_label)
      } else {
        r_res  <- a$res_stack[[y1]]  - a$res_stack[[y0]]
        r_nres <- a$nres_stack[[y1]] - a$nres_stack[[y0]]
        title_res  <- sprintf("Residential &Delta;<br>(m&sup2;/pixel, %s)<br><em>sqrt scale</em>",  yr_label)
        title_nres <- sprintf("Non-residential &Delta;<br>(m&sup2;/pixel, %s)<br><em>sqrt scale</em>", yr_label)
      }
      layer_res  <- "Residential change"
      layer_nres <- "Non-residential change"
      p_res  <- pal_res
      p_nres <- pal_nres
      upper  <- UPPER
      brks   <- legend_brks
    }

    y0i <- as.integer(y0); y1i <- as.integer(y1)
    bu_lookup <- function(yy) {
      v <- ts_data$area_total_km2[ts_data$slug == input$city & ts_data$year == yy]
      if (length(v)) v[1] else NA_real_
    }
    pop_lookup <- function(yy) {
      v <- pop_ts_data$pop[pop_ts_data$slug == input$city & pop_ts_data$year == yy]
      if (length(v)) v[1] else NA_real_
    }
    gs <- growth_stats(a, y0i, y1i)

    info_html <- city_info_html(
      name     = bb$agglosname,
      mode     = input$map_mode,
      y0       = y0i,
      y1       = y1i,
      bu_y0    = bu_lookup(y0i),
      bu_y1    = bu_lookup(y1i),
      pop_y0   = pop_lookup(y0i),
      pop_y1   = pop_lookup(y1i),
      tree_cov = tree_cov,
      gs       = gs,
      boundary_flag = bb$boundary_quality,
      growth_timing = bb$growth_timing,
      growth_form   = bb$growth_form
    )

    leaflet() |>
      addTiles(group = "OpenStreetMap") |>
      addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
      fitBounds(bb$xmin, bb$ymin, bb$xmax, bb$ymax) |>
      hideGroup("OpenStreetMap") |>
      addRasterImage(sqrt_capped(r_res,  upper = upper), colors = p_res,
                     opacity = 0.85, maxBytes = Inf,
                     group = layer_res) |>
      addRasterImage(sqrt_capped(r_nres, upper = upper), colors = p_nres,
                     opacity = 0.85, maxBytes = Inf,
                     group = layer_nres) |>
      addPolygons(
        data        = a$metro,
        fillColor   = pal_tree(ifelse(is.na(tree_cov), 0, tree_cov)),
        fillOpacity = 0,
        color = "#333", weight = 1.5, opacity = 1,
        group = "Metro outline",
        popup = as.character(info_html)
      ) |>
      addLayersControl(
        baseGroups    = c("Satellite", "OpenStreetMap"),
        overlayGroups = c(layer_res, layer_nres, "Metro outline"),
        options       = layersControlOptions(collapsed = FALSE)
      ) |>
      addLegend(colors   = p_res(sqrt(brks)),
                labels   = format(brks, big.mark = ","),
                title    = title_res,
                position = "bottomleft") |>
      addLegend(colors   = p_nres(sqrt(brks)),
                labels   = format(brks, big.mark = ","),
                title    = title_nres,
                position = "bottomleft") |>
      addControl(html = info_html, position = "topright")
  })

  # --- Nighttime Lights tab -------------------------------------------------
  observeEvent(input$ntl_country, {
    ch  <- cities_for(input$ntl_country)
    cur <- isolate(input$ntl_city)
    updateSelectInput(session, "ntl_city", choices = ch,
                      selected = if (isTRUE(cur %in% ch)) cur else ch[[1]])
  })

  ntl_assets <- reactive({
    req(input$ntl_city)
    load_ntl_assets(input$ntl_city)
  }) |> bindCache(input$ntl_city)

  ntl_city_bb <- reactive({
    city_index |> dplyr::filter(slug == input$ntl_city)
  })

  # Bare shell, rendered once. City / layer / year updates go through
  # leafletProxy so the year animation never reloads basemap tiles.
  output$ntl_map <- renderLeaflet({
    leaflet() |>
      addTiles(group = "OpenStreetMap") |>
      addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
      hideGroup("OpenStreetMap") |>
      setView(lng = 20, lat = 3, zoom = 3)
  })
  # Keep the map alive while the NTL tab is hidden, so proxy updates from the
  # observers below are not dropped before the user opens the tab.
  outputOptions(output, "ntl_map", suspendWhenHidden = FALSE)

  # City shell — view + metro outline + info box. Fires on city change only.
  observe({
    a  <- ntl_assets(); req(a)
    bb <- ntl_city_bb(); req(nrow(bb) == 1)

    info_html <- as.character(ntl_info_html(
      name         = bb$agglosname,
      ntl_2000     = bb$ntl_mean_2000,
      ntl_2025     = bb$ntl_mean_2025,
      darkdim_2000 = bb$darkdim_share_2000,
      darkdim_2025 = bb$darkdim_share_2025
    ))

    leafletProxy("ntl_map") |>
      clearGroup("Metro outline") |>
      removeControl("ntl_info") |>
      fitBounds(bb$xmin, bb$ymin, bb$xmax, bb$ymax) |>
      addPolygons(
        data        = a$metro,
        fillOpacity = 0,
        color = "#ffffff", weight = 1.5, opacity = 0.9,
        group = "Metro outline",
        popup = info_html
      ) |>
      addControl(html = info_html, position = "topright", layerId = "ntl_info")
  })

  # Raster for the current layer + year. Fires once per slider step (6 epochs).
  observe({
    a <- ntl_assets(); req(a)
    req(input$ntl_epoch, input$ntl_layer)
    yr <- as.integer(input$ntl_epoch)                  # one of 2000..2025 step 5

    p <- leafletProxy("ntl_map") |>
      clearGroup("NTL") |>
      removeControl("ntl_badge")

    if (input$ntl_layer == "ntl") {
      avail  <- as.integer(names(a$ntl_stack))         # annual 1992..2025
      ntl_yr <- min(max(yr, min(avail)), max(avail))
      r <- raster::raster(a$ntl_stack[[as.character(ntl_yr)]])
      v <- raster::values(r); v[v <= 0] <- NA
      r <- raster::setValues(r, log1p(pmin(v, NTL_UPPER)))
      p |> addRasterImage(r, colors = pal_ntl, opacity = 0.8,
                          maxBytes = Inf, group = "NTL")
      badge <- as.character(yr)
    } else {
      ep <- as.character(yr)                           # lu_stack has 2000..2025
      if (!(ep %in% names(a$lu_stack))) ep <- tail(names(a$lu_stack), 1)
      r  <- raster::raster(a$lu_stack[[ep]])
      v  <- raster::values(r); v[v == 0] <- NA
      r  <- raster::setValues(r, v)
      p |> addRasterImage(r, colors = pal_lu, opacity = 0.75,
                          maxBytes = Inf, group = "NTL")
      badge <- ep
    }

    leafletProxy("ntl_map") |>
      addControl(
        html = sprintf(
          paste0("<div style=\"font:700 30px/1 -apple-system,system-ui,sans-serif;",
                 "color:#1a1a1a;background:rgba(255,255,255,0.78);padding:3px 12px;",
                 "border-radius:6px;box-shadow:0 1px 4px rgba(0,0,0,.3)\">%s</div>"),
          badge),
        position = "bottomright", layerId = "ntl_badge")
  })

  # Legend + layers control — depend on the layer choice only.
  observe({
    req(input$ntl_layer)
    p <- leafletProxy("ntl_map") |>
      removeControl("ntl_legend") |>
      addLayersControl(
        baseGroups    = c("Satellite", "OpenStreetMap"),
        overlayGroups = c("NTL", "Metro outline"),
        options       = layersControlOptions(collapsed = FALSE))

    if (input$ntl_layer == "ntl") {
      p |> addLegend(layerId  = "ntl_legend",
                     colors   = pal_ntl(pmin(log1p(NTL_BREAKS), log1p(NTL_UPPER))),
                     labels   = NTL_LABELS,
                     title    = sprintf("Mean NTL<br>(corrected DN)<br><em>log, cap %d</em>", NTL_UPPER),
                     position = "bottomleft")
    } else {
      p |> addLegend(layerId  = "ntl_legend",
                     colors   = LU_COLS,
                     labels   = LU_LABELS,
                     title    = "Built-up light class",
                     position = "bottomleft")
    }
  })

  # --- Time-series tab ------------------------------------------------------
  observeEvent(input$ts_country, {
    ch  <- cities_for(input$ts_country)
    cur <- isolate(input$ts_city)
    updateSelectInput(session, "ts_city", choices = ch,
                      selected = if (isTRUE(cur %in% ch)) cur else ch[[1]])
  })

  observeEvent(input$ts_add_city, {
    req(input$ts_city)
    new_sel <- unique(c(isolate(input$cities_ts), input$ts_city))
    updateSelectizeInput(session, "cities_ts", selected = new_sel)
  })

  output$ts_plot <- renderPlot({
    req(input$cities_ts)
    scl            <- input$ts_scale
    selected_slugs <- input$cities_ts

    # Consistent city order: descending by final-year total built-up
    city_order <- city_index |>
      dplyr::filter(slug %in% selected_slugs) |>
      dplyr::left_join(
        ts_data |>
          dplyr::filter(year == max(year)) |>
          dplyr::select(slug, area_total_km2),
        by = "slug"
      ) |>
      dplyr::arrange(dplyr::desc(dplyr::coalesce(area_total_km2, 0))) |>
      dplyr::pull(agglosname)

    n_cities  <- max(length(city_order), 1L)
    base_cols <- scales::brewer_pal(palette = "Dark2")(min(n_cities, 8))
    city_cols <- setNames(
      if (n_cities <= 8) base_cols else colorRampPalette(base_cols)(n_cities),
      city_order
    )

    # GHSL data (8 epochs: 1990–2025)
    df_ghsl <- ts_data |>
      dplyr::filter(slug %in% selected_slugs) |>
      dplyr::mutate(agglosname = factor(agglosname, levels = city_order))

    if (scl == "idx") {
      base_vals <- df_ghsl |>
        dplyr::filter(year == min(year)) |>
        dplyr::select(slug,
                      b_tot  = area_total_km2,
                      b_res  = area_res_km2,
                      b_nres = area_nres_km2)
      df_ghsl <- df_ghsl |>
        dplyr::left_join(base_vals, by = "slug") |>
        dplyr::mutate(
          area_total_km2 = dplyr::if_else(is.na(b_tot)  | b_tot  == 0, NA_real_, area_total_km2 / b_tot  * 100),
          area_res_km2   = dplyr::if_else(is.na(b_res)  | b_res  == 0, NA_real_, area_res_km2   / b_res  * 100),
          area_nres_km2  = dplyr::if_else(is.na(b_nres) | b_nres == 0, NA_real_, area_nres_km2  / b_nres * 100)
        ) |>
        dplyr::select(-b_tot, -b_res, -b_nres)
    } else if (scl == "pop") {
      # Time-varying GHS-POP denominator (year-matched), not a fixed pop2020.
      df_ghsl <- df_ghsl |>
        dplyr::left_join(pop_ts_data |> dplyr::select(slug, year, pop),
                         by = c("slug", "year")) |>
        dplyr::mutate(
          area_total_km2 = area_total_km2 / (pop / 1000),
          area_res_km2   = area_res_km2   / (pop / 1000),
          area_nres_km2  = area_nres_km2  / (pop / 1000)
        ) |>
        dplyr::select(-pop)
    }

    ghsl_units <- switch(scl,
      abs = "km²",
      idx = "index (1990 = 100)",
      pop = "km² per 1,000 res."
    )

    # NTL data — clipped to 2000–2025 to align with the built-up / population
    # panels (the corrected DMSP series runs 1992–2025; pre-2000 is noisier).
    df_ntl <- ntl_ts_data |>
      dplyr::filter(slug %in% selected_slugs, year >= 2000) |>
      dplyr::mutate(
        agglosname  = factor(agglosname, levels = city_order),
        darkdim_pct = darkdim_share * 100
      )

    # Population data (GHS-POP, 1990–2025)
    df_pop <- pop_ts_data |>
      dplyr::filter(slug %in% selected_slugs) |>
      dplyr::mutate(agglosname = factor(agglosname, levels = city_order),
                    pop_m      = pop / 1e6)

    make_panel <- function(df, y, y_label, x_breaks, caption = NULL) {
      ggplot(df, aes(year, .data[[y]], colour = agglosname)) +
        geom_line(linewidth = 0.8) +
        geom_point(size = 1.5) +
        scale_colour_manual(values = city_cols, drop = FALSE) +
        scale_x_continuous(breaks = x_breaks) +
        scale_y_continuous(labels = scales::label_comma()) +
        labs(x = NULL, y = y_label, colour = NULL, caption = caption) +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank(),
              plot.caption = element_text(size = 8, colour = "grey55", hjust = 0))
    }

    ghsl_brks <- c(1990, 2000, 2010, 2020, 2025)   # built-up + population panels span 1990–2025
    ntl_brks  <- c(2000, 2005, 2010, 2015, 2020, 2025)

    builtup_col   <- switch(input$ts_builtup_type,
      total = "area_total_km2",
      res   = "area_res_km2",
      nres  = "area_nres_km2"
    )
    builtup_label <- switch(input$ts_builtup_type,
      total = paste("Total built-up,",     ghsl_units),
      res   = paste("Residential,",        ghsl_units),
      nres  = paste("Non-residential,",    ghsl_units)
    )

    p_builtup <- make_panel(df_ghsl, builtup_col, builtup_label, ghsl_brks,
                            caption = "Source: GHSL-BUILT-S R2023")
    p_pop     <- make_panel(df_pop, "pop_m",
                            "Population (millions)", ghsl_brks,
                            caption = "Source: GHSL GHS-POP R2023A")
    p_ntl     <- make_panel(df_ntl, "ntl_mean",
                            "Mean NTL (corrected DN)", ntl_brks,
                            caption = "Source: DMSP bloom/top-code-corrected (Chiovelli et al. 2026)")
    p_darkdim <- make_panel(df_ntl, "darkdim_pct",
                            "Dark or dim built-up share (%)", ntl_brks)

    patchwork::wrap_plots(p_builtup, p_pop, p_ntl, p_darkdim, ncol = 2) +
      patchwork::plot_layout(guides = "collect") &
      theme(legend.position = "right")
  })

  # --- Rankings tab -----------------------------------------------------------
  rank_df <- reactive({
    view <- if (isTRUE(input$rank_view %in% names(rank_tables))) input$rank_view else "Built-up"
    rank_tables[[view]]
  })

  output$league_table <- DT::renderDataTable({
    df       <- rank_df()
    bar_col  <- unname(rank_sort[if (isTRUE(input$rank_view %in% names(rank_sort))) input$rank_view else "Built-up"])
    bar_idx  <- match(bar_col, names(df)) - 1L   # 0-based for DT

    dt <- DT::datatable(
      df,
      rownames   = FALSE,
      filter     = "top",
      options    = list(
        pageLength = 25,
        order      = list(list(bar_idx, "desc"))
      )
    )
    if (!is.na(bar_idx) && is.numeric(df[[bar_col]])) {
      dt <- dt |> DT::formatStyle(
        bar_col,
        background         = DT::styleColorBar(range(df[[bar_col]], na.rm = TRUE), "#b2e2f7"),
        backgroundSize     = "98% 60%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      )
    }
    dt
  }, server = FALSE)

  output$dl_table <- downloadHandler(
    filename = function() {
      v   <- if (isTRUE(input$rank_view %in% names(rank_tables))) input$rank_view else "built up"
      tag <- gsub("[^a-z]+", "_", tolower(v))
      paste0("urban_africa_rankings_", tag, "_", Sys.Date(), ".csv")
    },
    content  = function(file) write.csv(rank_df(), file, row.names = FALSE)
  )

  # --- Scatterplots tab -------------------------------------------------------
  output$scatter <- plotly::renderPlotly({

    if (input$scatter_type == "extent_growth") {
      df <- city_index |>
        dplyr::filter(!is.na(pop2020), !is.na(total_km2_1990),
                      !is.na(delta_total_km2_1990_2025))

      p <- ggplot(df,
                  aes(x      = total_km2_1990,
                      y      = delta_total_km2_1990_2025,
                      size   = pop2020 / 1e6,
                      colour = macro_region,
                      text   = paste0(
                        agglosname, " (", iso3, ")\n",
                        "Built-up 1990: ", round(total_km2_1990, 0), " km²\n",
                        "Growth 1990–2025: +", round(delta_total_km2_1990_2025, 0),
                        " km²", sprintf(" (+%.0f%%)", pct_growth * 100)
                      ))) +
        geom_point(alpha = 0.8) +
        scale_x_continuous(labels = scales::label_comma(), trans = "sqrt") +
        scale_y_continuous(labels = scales::label_comma(), trans = "sqrt") +
        scale_size_continuous(name = "Pop. 2020\n(millions)", range = c(2, 12)) +
        scale_colour_viridis_d(option = "D", end = 0.9, name = "Region", drop = FALSE) +
        labs(
          x     = "Built-up extent 1990 (km², sqrt scale)",
          y     = "Built-up growth 1990–2025 (km², sqrt scale)",
          title = "Initial built-up extent vs. growth — 100 largest African agglomerations"
        ) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "right", panel.grid.minor = element_blank())

    } else if (input$scatter_type == "sprawl_intens") {
      df <- city_index |>
        dplyr::filter(!is.na(sprawl_km2), !is.na(intens_km2))

      p <- ggplot(df,
                  aes(x      = intens_km2,
                      y      = sprawl_km2,
                      size   = delta_total_km2_1990_2025,
                      colour = macro_region,
                      text   = paste0(
                        agglosname, " (", iso3, ")\n",
                        "New land:        ", round(sprawl_km2, 1),
                        " km² (", round(sprawl_share * 100), "%)\n",
                        "Densification: ", round(intens_km2, 1),
                        " km² (", round((1 - sprawl_share) * 100), "%)\n",
                        "Density change: ", sprintf("%+.3f", density_change)
                      ))) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
        geom_point(alpha = 0.8) +
        scale_x_continuous(labels = scales::label_comma(), trans = "sqrt") +
        scale_y_continuous(labels = scales::label_comma(), trans = "sqrt") +
        scale_size_continuous(name = "Total growth\n(km²)", range = c(2, 12)) +
        scale_colour_viridis_d(option = "D", end = 0.9, name = "Region", drop = FALSE) +
        labs(
          x     = "Densification — growth within existing footprint (km², sqrt scale)",
          y     = "New land — growth on previously unbuilt land (km², sqrt scale)",
          title = "Sprawl vs. intensification 1990–2025 — above diagonal = sprawl-dominant"
        ) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "right", panel.grid.minor = element_blank())

    } else {
      df <- city_index |>
        dplyr::filter(!is.na(pop_ghs_growth), !is.na(pct_growth), pop_ghs_growth > 0)

      p <- ggplot(df,
                  aes(x      = pop_ghs_growth * 100,
                      y      = pct_growth * 100,
                      size   = total_km2_2025,
                      colour = macro_region,
                      text   = paste0(
                        agglosname, " (", iso3, ")\n",
                        "Population growth: +", round(pop_ghs_growth * 100), "%\n",
                        "Built-up growth: +", round(pct_growth * 100), "%\n",
                        "Land/pop growth ratio: ", sprintf("%.2f", pct_growth / pop_ghs_growth)
                      ))) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
        geom_point(alpha = 0.8) +
        scale_x_continuous(labels = scales::label_comma(), trans = "sqrt") +
        scale_y_continuous(labels = scales::label_comma(), trans = "sqrt") +
        scale_size_continuous(name = "Built-up\n2025 (km²)", range = c(2, 12)) +
        scale_colour_viridis_d(option = "D", end = 0.9, name = "Region", drop = FALSE) +
        labs(
          x     = "Population growth 1990–2025 (%, sqrt scale)",
          y     = "Built-up growth 1990–2025 (%, sqrt scale)",
          title = "Population growth vs. built-up growth — above diagonal = land outran population"
        ) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "right", panel.grid.minor = element_blank())
    }

    plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(legend = list(orientation = "v"))
  })
}

shinyApp(ui, server)
