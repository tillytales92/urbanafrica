# app/app.R
# Urban Africa Shiny app — built-up growth maps + NTL + time series.
#
# Launch:  shiny::runApp(here::here("app"), launch.browser = TRUE)

pacman::p_load(shiny, bslib, leaflet, sf, here, dplyr, countrycode, ggplot2,
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
    subregion  = countrycode::countrycode(iso3, "iso3c", "un.regionsub.name"),
    pct_growth = delta_total_km2_2000_2025 / total_km2_2000
  ) |>
  dplyr::arrange(country, agglosname)

# Join tree cover % (pre-built by 0_simplifyshapefile.R)
tree_cover   <- readRDS("data/agglom_attrs.Rds")
sprawl_stats <- readRDS("data/sprawl_stats.Rds")
city_index <- city_index |>
  dplyr::left_join(tree_cover,   by = "slug") |>
  dplyr::left_join(
    sprawl_stats |> dplyr::select(slug, sprawl_km2, intens_km2, sprawl_share,
                                   footprint_2000_km2, footprint_2025_km2,
                                   density_2000, density_2025, density_change),
    by = "slug"
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

# Annual NTL time-series 2000-2024 (25 years × 100 cities).
ntl_ts_data <- readRDS("data/africapolis_ntl.Rds") |>
  dplyr::inner_join(
    city_index |> dplyr::select(slug, iso3, agglosname),
    by = c("iso3", "agglosname")
  ) |>
  dplyr::mutate(unlit_share = 1 - ntl_lit_share)

# Pre-compute key NTL statistics and join into city_index for info boxes.
ntl_stats_2000 <- ntl_ts_data |>
  dplyr::filter(year == 2000) |>
  dplyr::select(slug, ntl_mean_2000 = ntl_mean, unlit_share_2000 = unlit_share)
ntl_stats_2020 <- ntl_ts_data |>
  dplyr::filter(year == 2020) |>
  dplyr::select(slug, ntl_mean_2020 = ntl_mean)
ntl_stats_2024 <- ntl_ts_data |>
  dplyr::filter(year == 2024) |>
  dplyr::select(slug, ntl_mean_2024 = ntl_mean)

city_index <- city_index |>
  dplyr::left_join(ntl_stats_2000, by = "slug") |>
  dplyr::left_join(ntl_stats_2020, by = "slug") |>
  dplyr::left_join(ntl_stats_2024, by = "slug")

# Multi-select choices: "City (ISO3)" → slug, so duplicate names disambiguate.
all_city_choices <- setNames(
  city_index$slug,
  paste0(city_index$agglosname, " (", city_index$iso3, ")")
)

# Pre-formatted table for the Rankings tab (built once at startup).
league_df <- city_index |>
  dplyr::arrange(dplyr::desc(pct_growth)) |>
  dplyr::transmute(
    City                  = agglosname,
    Country               = country,
    `Sub-region`          = subregion,
    `Built-up 2000 (km²)` = round(total_km2_2000, 1),
    `Built-up 2025 (km²)` = round(total_km2_2025, 1),
    `Change (km²)`        = round(delta_total_km2_2000_2025, 1),
    `Growth (%)`          = round(pct_growth * 100, 1),
    `Sprawl (%)`          = round(sprawl_share * 100, 1),
    `Density change`      = round(density_change, 3),
    `Tree cover (%)`      = round(p_tree_cov, 1),
    `Unlit share (%)`     = round(unlit_share_2025 * 100, 1)
  )

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
epochs      <- c(2000, 2005, 2010, 2015, 2020, 2025)
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

# NTL & lit/unlit palettes
# Breaks anchored at the lit/unlit threshold (0.5); steps match the African
# city distribution: median ~6.5, p75 ~17, p90 ~33, p99 ~71 nW/cm²/sr (2024).
NTL_UPPER    <- 200
NTL_BREAKS   <- c(0, 0.5, 5, 20, 60, NTL_UPPER)
NTL_LABELS   <- c("0", "0.5 (lit threshold)", "5", "20", "60", "200+")
# log1p transform spreads low-value pixels across the palette (most African city
# pixels sit under ~30 nW/cm²/sr; linear 0–200 would render them all near-black).
# The legend still shows original-scale labels; log1p is applied to the break
# values when requesting colours from the palette.
pal_ntl      <- colorNumeric("inferno", domain = c(0, log1p(NTL_UPPER)), na.color = "transparent")
pal_lu       <- colorFactor(c("#e34a33", "#fee391"), levels = c(1, 2), na.color = "transparent")

sqrt_capped <- function(r, upper = UPPER) {
  if (inherits(r, "SpatRaster")) r <- raster::raster(r)
  v <- raster::values(r)
  v[v <= 0] <- NA
  v <- sqrt(pmin(v, upper))
  raster::setValues(r, v)
}

# Info panel for the Urban Growth tab.
# Shows built-up stats, sprawl decomposition, tree cover and population (Africapolis 2020).
# Unlit built-up is shown in the NTL tab instead.
city_info_html <- function(name, km2_2000, km2_2025, delta, pct,
                           tree_cov, pop2020 = NA,
                           sprawl_km2 = NA, intens_km2 = NA, sprawl_share = NA,
                           density_change = NA) {
  tree_str <- if (is.na(tree_cov)) "N/A" else sprintf("%.1f%%", tree_cov)
  pop_str  <- if (is.na(pop2020))  "N/A" else format(round(pop2020), big.mark = ",", scientific = FALSE)

  sprawl_row <- if (!is.na(sprawl_share)) {
    paste0(
      "<tr style='border-top:1px solid #ddd'>",
      "<td style='color:#555;padding-right:8px;padding-top:4px'>New land (sprawl)</td>",
      "<td style='text-align:right;padding-top:4px'>",
      sprintf("%.1f km&sup2; (%.0f%%)", sprawl_km2, sprawl_share * 100), "</td></tr>",
      "<tr><td style='color:#555;padding-right:8px'>Densification</td>",
      "<td style='text-align:right'>",
      sprintf("%.1f km&sup2; (%.0f%%)", intens_km2, (1 - sprawl_share) * 100), "</td></tr>",
      "<tr><td style='color:#555;padding-right:8px'>Density trend*</td>",
      "<td style='text-align:right'>",
      if (is.na(density_change)) "N/A"
      else if (density_change > 0) sprintf("+%.3f (densifying)", density_change)
      else sprintf("%.3f (sprawling)", density_change),
      "</td></tr>"
    )
  } else ""

  footnote <- if (!is.na(sprawl_share)) {
    paste0(
      "<p style='margin:6px 0 0;font-size:10px;color:#888;",
      "border-top:1px solid #eee;padding-top:4px'>",
      "* Density trend = built-up area &divide; built-up footprint (both km&sup2;).<br>",
      "Positive = densifying; negative = footprint expanding faster than surface.",
      "</p>"
    )
  } else ""

  HTML(paste0(
    "<div style='background:rgba(255,255,255,0.95);border-radius:6px;",
    "box-shadow:0 1px 5px rgba(0,0,0,0.4);min-width:220px;",
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
    "<tr><td style='color:#555;padding-right:8px'>Built-up 2000</td>",
    "<td style='text-align:right'>", sprintf("%.1f km&sup2;", km2_2000), "</td></tr>",
    "<tr><td style='color:#555;padding-right:8px'>Built-up 2025</td>",
    "<td style='text-align:right'>", sprintf("%.1f km&sup2;", km2_2025), "</td></tr>",
    "<tr style='border-top:1px solid #ddd'>",
    "<td style='color:#555;padding-right:8px;padding-top:4px'>Increase 2000–2025</td>",
    "<td style='text-align:right;padding-top:4px;font-weight:600'>",
    sprintf("+%.1f km&sup2; (+%.0f%%)", delta, pct * 100), "</td></tr>",
    sprawl_row,
    "<tr style='border-top:1px solid #ddd'>",
    "<td style='color:#555;padding-right:8px;padding-top:4px'>Tree cover (2020)</td>",
    "<td style='text-align:right;padding-top:4px'>", tree_str, "</td></tr>",
    "<tr><td style='color:#555;padding-right:8px'>Population (2020)</td>",
    "<td style='text-align:right'>", pop_str, "</td></tr>",
    "</table>",
    footnote,
    "</div></div>"
  ))
}

# Info panel for the Nighttime Lights tab.
# Shows NTL intensity (2020 / 2025 proxy / change) and unlit built-up share
# (2000 / 2025 proxy / change).
ntl_info_html <- function(name, ntl_2020, ntl_2024,
                           unlit_2000, unlit_2025) {
  fmt_ntl <- function(x) if (is.na(x)) "N/A" else sprintf("%.2f", x)
  ntl_delta <- if (is.na(ntl_2020) || is.na(ntl_2024)) NA_real_
               else ntl_2024 - ntl_2020
  ntl_delta_str <- if (is.na(ntl_delta)) "N/A"
                   else sprintf("%+.2f", ntl_delta)

  fmt_pct <- function(x) if (is.na(x)) "N/A" else sprintf("%.1f%%", x * 100)
  unlit_delta_pp <- if (is.na(unlit_2000) || is.na(unlit_2025)) NA_real_
                    else (unlit_2025 - unlit_2000) * 100
  unlit_delta_str <- if (is.na(unlit_delta_pp)) "N/A"
                     else sprintf("%+.1f pp", unlit_delta_pp)

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
    "NTL intensity (nW/cm&sup2;/sr)</td></tr>",
    "<tr><td style='color:#555;padding-right:8px'>2020</td>",
    "<td style='text-align:right'>", fmt_ntl(ntl_2020), "</td></tr>",
    "<tr><td style='color:#555;padding-right:8px'>2025 (2024 proxy)</td>",
    "<td style='text-align:right'>", fmt_ntl(ntl_2024), "</td></tr>",
    "<tr style='border-bottom:1px solid #ddd'>",
    "<td style='color:#555;padding-right:8px'>Change</td>",
    "<td style='text-align:right;font-weight:600'>", ntl_delta_str, "</td></tr>",
    "<tr><td colspan='2' style='color:#444;font-weight:600;",
    "padding-top:6px;padding-bottom:2px'>Unlit built-up share</td></tr>",
    "<tr><td style='color:#555;padding-right:8px'>2000</td>",
    "<td style='text-align:right'>", fmt_pct(unlit_2000), "</td></tr>",
    "<tr><td style='color:#555;padding-right:8px'>2025 (2024 proxy)</td>",
    "<td style='text-align:right'>", fmt_pct(unlit_2025), "</td></tr>",
    "<tr><td style='color:#555;padding-right:8px'>Change</td>",
    "<td style='text-align:right;font-weight:600'>", unlit_delta_str, "</td></tr>",
    "</table>",
    "<p style='margin:6px 0 0;font-size:10px;color:#888;border-top:1px solid #eee;padding-top:4px'>",
    "Lit threshold: 0.5 nW/cm&sup2;/sr. Unlit = informal / low-density built-up.",
    "</p>",
    "</div></div>"
  ))
}

load_assets <- function(slug) {
  if (is.null(cities_root)) return(NULL)
  d <- file.path(cities_root, slug)
  if (!dir.exists(d)) return(NULL)
  list(
    metro      = sf::st_read(file.path(d, "metro.gpkg"), quiet = TRUE),
    res_stack  = terra::rast(file.path(d, "res_wgs84.tif")),
    nres_stack = terra::rast(file.path(d, "nres_wgs84.tif"))
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
                  selected = 2000,
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
  helpText("NTL intensity and unlit share are always shown in absolute units.")
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
              min = 2000, max = 2024, value = 2024, step = 1,
              sep = "", ticks = FALSE,
              animate = animationOptions(interval = 800, loop = TRUE)),
  helpText("Press play to sweep 2000 → 2024. NTL intensity is annual; ",
           "lit/unlit snaps to the nearest 5-year epoch (2025 uses 2024 data).")
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
        plotOutput("ts_plot", height = "900px")
      )
    )
  ),
  nav_panel(
    "Rankings",
    div(
      style = "padding: 12px",
      div(
        style = "margin-bottom: 10px; display: flex; justify-content: space-between; align-items: center",
        h5("Built-up growth ranking — 100 largest African agglomerations", style = "margin: 0"),
        downloadButton("dl_table", "Download CSV", class = "btn-sm btn-outline-secondary")
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
            "Initial extent vs. growth"  = "extent_growth",
            "Sprawl vs. intensification" = "sprawl_intens"
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
        tags$b("100 largest African agglomerations"), " (by 2020 population) from 2000 to 2025.",
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
          " The info panel shows built-up totals, sprawl decomposition, tree cover, and population."
        ),
        tags$dt(tags$b("Nighttime Lights")),
        tags$dd(
          "Annual NTL intensity raster (log-scaled, capped at 200 nW/cm²/sr) or a pixel-level",
          " lit / unlit classification for any epoch 2000–2025.",
          " The info panel shows city-level NTL means and unlit built-up shares for 2020 and 2024."
        ),
        tags$dt(tags$b("Time Series")),
        tags$dd(
          "Multi-city line charts for built-up area (total, residential, or non-residential),",
          " mean NTL intensity, and unlit built-up share.",
          " Built-up can be shown in absolute km², as a growth index (2000 = 100),",
          " or per 1,000 residents.",
          " Use the country / city picker and ", tags$em("Add to comparison"), " button to build",
          " a custom comparison set."
        ),
        tags$dt(tags$b("Rankings")),
        tags$dd(
          "Sortable table of all 100 agglomerations with built-up extent (2000 & 2025),",
          " absolute and percentage growth, sprawl share, density change, tree cover, and",
          " unlit built-up share. Filterable and downloadable as CSV."
        ),
        tags$dt(tags$b("Scatterplots")),
        tags$dd(
          tags$em("Initial extent vs. growth:"), " identifies whether larger cities grew more in absolute terms.",
          tags$br(),
          tags$em("Sprawl vs. intensification:"), " plots new-land growth against within-footprint",
          " densification; cities above the diagonal are sprawl-dominant."
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
          " Six epochs: 2000, 2005, 2010, 2015, 2020, 2025.",
          " Separate layers for total, residential, and non-residential built-up.",
          " The 2025 layer is model-extrapolated, not directly observed."
        ),
        tags$li(
          tags$b("Africapolis 2020 (agglomeration boundaries):"),
          " Urban agglomeration polygons, 2020 population estimates, and tree-cover percentage",
          " for African agglomerations (OECD/Sahel and West Africa Club, SWAC).",
          " Provides the spatial units used to aggregate all raster statistics."
        ),
        tags$li(
          tags$b("NASA Black Marble VNL v2.2 (VIIRS, 2013–2024):"),
          " Annual nighttime light composites at ~500 m resolution from the",
          " Visible Infrared Imaging Radiometer Suite (VIIRS) aboard Suomi-NPP / NOAA-20.",
          " Radiometrically corrected and cloud-screened."
        ),
        tags$li(
          tags$b("Li et al. (2020) — harmonised DMSP-OLS (2000–2013):"),
          " Intercalibrated annual composites from the Defense Meteorological Satellite Program",
          " Operational Linescan System (DMSP-OLS), harmonised to reduce inter-satellite",
          " and inter-annual inconsistencies.",
          " Bridges the pre-VIIRS period; the 2013 overlap year is used to align the two series."
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

      tags$h5("Sprawl decomposition (2000–2025)"),
      p(
        "Total growth is decomposed into sprawl (extensive) and densification (intensive) km².",
        " The ", tags$b("sprawl share"), " is the fraction of net new built-up surface that",
        " came from new pixels rather than intensification of existing ones.",
        " A shrinkage component (pixels where surface declined) is tracked separately but",
        " is negligible for most cities."
      ),

      tags$h5("Density trend"),
      p(
        "The ", tags$b("density index"), " is defined as total built-up surface (km²) divided by",
        " the built-up footprint (km², the count of pixels with any built-up surface × 0.01 km²/pixel).",
        " A rising density index means built-up surface is growing faster than the footprint —",
        " the city is filling in. A falling index means the footprint is expanding faster than",
        " the surface — built form is spreading thin.",
        " The ", tags$b("density change"), " reported in the app is the 2025 index minus the 2000 index;"  ,
        " positive = densifying, negative = sprawling."
      ),

      tags$h5("Nighttime light classification"),
      p(
        "Each built-up pixel is classified as ", tags$b("lit"), " (NTL > 0.5 nW/cm²/sr) or",
        tags$b(" unlit"), " based on the annual NTL composite for the same year.",
        " The 0.5 nW/cm²/sr threshold is the standard lit/unlit boundary used in the",
        " Black Marble product documentation.",
        " Unlit built-up serves as a proxy for informal or low-density settlements that",
        " lack sufficient artificial lighting to be detected by VIIRS.",
        " NTL maps are displayed on a log₁p scale (values capped at 200 nW/cm²/sr)",
        " to spread the low-value pixels that dominate African cities."
      ),

      p(
        tags$em(
          "Note: for 2025 figures, the most recent available NTL year (2024) is used as a proxy.",
          " DMSP-OLS (2000–2012) and VIIRS (2013–2024) DN values are not directly comparable;",
          " the Li et al. harmonisation reduces, but does not eliminate, sensor discontinuities."
        ),
        style = "font-size:12px; color:#666; border-left:3px solid #ddd; padding-left:10px; margin-top:4px"
      ),

      hr(),
      h4("Units & display scales"),
      tags$ul(
        tags$li("All area figures: ", tags$b("km²")),
        tags$li("NTL intensity: ", tags$b("nW/cm²/sr"), " (nanowatts per cm² per steradian)"),
        tags$li(
          "Built-up change maps: ", tags$b("sqrt scale"),
          " — the raw change distribution is heavily right-skewed (most pixels show modest change;",
          " a small fraction show very large values). Square-root scaling compresses outliers",
          " and reveals spatial patterns across the full range. Zero-change pixels are transparent."
        ),
        tags$li(
          "NTL intensity maps: ", tags$b("log₁p scale"),
          " — most urban pixels in Africa sit below ~30 nW/cm²/sr;",
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

    info_html <- city_info_html(
      name           = bb$agglosname,
      km2_2000       = bb$total_km2_2000,
      km2_2025       = bb$total_km2_2025,
      delta          = bb$delta_total_km2_2000_2025,
      pct            = bb$pct_growth,
      tree_cov       = tree_cov,
      pop2020        = bb$pop2020,
      sprawl_km2     = bb$sprawl_km2,
      intens_km2     = bb$intens_km2,
      sprawl_share   = bb$sprawl_share,
      density_change = bb$density_change
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
      name        = bb$agglosname,
      ntl_2020    = bb$ntl_mean_2020,
      ntl_2024    = bb$ntl_mean_2024,
      unlit_2000  = bb$unlit_share_2000,
      unlit_2025  = bb$unlit_share_2025
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

  # Raster for the current layer + year. Fires on every slider step.
  observe({
    a <- ntl_assets(); req(a)
    req(input$ntl_epoch, input$ntl_layer)
    yr_req <- as.integer(input$ntl_epoch)

    p <- leafletProxy("ntl_map") |>
      clearGroup("NTL") |>
      removeControl("ntl_badge")

    if (input$ntl_layer == "ntl") {
      avail <- as.integer(names(a$ntl_stack))
      yr    <- max(min(yr_req, max(avail)), min(avail))
      r <- raster::raster(a$ntl_stack[[as.character(yr)]])
      v <- raster::values(r); v[v <= 0] <- NA
      r <- raster::setValues(r, log1p(pmin(v, NTL_UPPER)))
      p |> addRasterImage(r, colors = pal_ntl, opacity = 0.8,
                          maxBytes = Inf, group = "NTL")
      badge <- as.character(yr)
    } else {
      ep <- epochs[which.min(abs(epochs - yr_req))]     # nearest 5-year epoch
      r  <- raster::raster(a$lu_stack[[as.character(ep)]])
      v  <- raster::values(r); v[v == 0] <- NA
      r  <- raster::setValues(r, v)
      p |> addRasterImage(r, colors = pal_lu, opacity = 0.75,
                          maxBytes = Inf, group = "NTL")
      badge <- if (ep == 2025) "2025 · 2024 data" else as.character(ep)
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
                     colors   = pal_ntl(log1p(NTL_BREAKS)),
                     labels   = NTL_LABELS,
                     title    = sprintf("NTL intensity<br>(nW/cm&sup2;/sr)<br><em>log, cap %d</em>", NTL_UPPER),
                     position = "bottomleft")
    } else {
      p |> addLegend(layerId  = "ntl_legend",
                     colors   = c("#e34a33", "#fee391"),
                     labels   = c("Built — unlit", "Built — lit"),
                     title    = "Built-up type",
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

    # GHSL data (6 epochs: 2000–2025)
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
      df_ghsl <- df_ghsl |>
        dplyr::left_join(city_index |> dplyr::select(slug, pop2020), by = "slug") |>
        dplyr::mutate(
          area_total_km2 = area_total_km2 / (pop2020 / 1000),
          area_res_km2   = area_res_km2   / (pop2020 / 1000),
          area_nres_km2  = area_nres_km2  / (pop2020 / 1000)
        )
    }

    ghsl_units <- switch(scl,
      abs = "km²",
      idx = "index (2000 = 100)",
      pop = "km² per 1,000 res."
    )

    # NTL data (annual 2000–2024)
    df_ntl <- ntl_ts_data |>
      dplyr::filter(slug %in% selected_slugs) |>
      dplyr::mutate(
        agglosname = factor(agglosname, levels = city_order),
        unlit_pct  = unlit_share * 100
      )

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

    ghsl_brks <- c(2000, 2005, 2010, 2015, 2020, 2025)
    ntl_brks  <- seq(2000, 2024, by = 4)

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
                            caption = "Source: GHSL R2023")
    p_ntl     <- make_panel(df_ntl, "ntl_mean",
                            "NTL intensity (nW/cm²/sr)", ntl_brks,
                            caption = "Source: NASA Black Marble VNL v2.2 / Li et al. (2020)")
    p_unlit   <- make_panel(df_ntl, "unlit_pct",
                            "Unlit built-up share (%)", ntl_brks)

    patchwork::wrap_plots(p_builtup, p_ntl, p_unlit, ncol = 1) +
      patchwork::plot_layout(guides = "collect") &
      theme(legend.position = "right")
  })

  # --- Rankings tab -----------------------------------------------------------
  output$league_table <- DT::renderDataTable({
    DT::datatable(
      league_df,
      rownames   = FALSE,
      filter     = "top",
      options    = list(
        pageLength = 25,
        order      = list(list(6L, "desc"))
      )
    ) |>
      DT::formatStyle(
        "Growth (%)",
        background         = DT::styleColorBar(
          range(league_df[["Growth (%)"]], na.rm = TRUE), "#b2e2f7"),
        backgroundSize     = "98% 60%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      )
  }, server = FALSE)

  output$dl_table <- downloadHandler(
    filename = function() paste0("urban_africa_rankings_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(league_df, file, row.names = FALSE)
  )

  # --- Scatterplots tab -------------------------------------------------------
  output$scatter <- plotly::renderPlotly({

    if (input$scatter_type == "extent_growth") {
      df <- city_index |>
        dplyr::filter(!is.na(pop2020), !is.na(total_km2_2000),
                      !is.na(delta_total_km2_2000_2025))

      p <- ggplot(df,
                  aes(x      = total_km2_2000,
                      y      = delta_total_km2_2000_2025,
                      size   = pop2020 / 1e6,
                      colour = subregion,
                      text   = paste0(
                        agglosname, " (", iso3, ")\n",
                        "Built-up 2000: ", round(total_km2_2000, 0), " km²\n",
                        "Growth 2000–2025: +", round(delta_total_km2_2000_2025, 0),
                        " km²", sprintf(" (+%.0f%%)", pct_growth * 100)
                      ))) +
        geom_point(alpha = 0.8) +
        scale_x_continuous(labels = scales::label_comma(), trans = "sqrt") +
        scale_y_continuous(labels = scales::label_comma(), trans = "sqrt") +
        scale_size_continuous(name = "Pop. 2020\n(millions)", range = c(2, 12)) +
        scale_colour_viridis_d(option = "D", end = 0.9, name = "Sub-region") +
        labs(
          x     = "Built-up extent 2000 (km², sqrt scale)",
          y     = "Built-up growth 2000–2025 (km², sqrt scale)",
          title = "Initial built-up extent vs. growth — 100 largest African agglomerations"
        ) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "right", panel.grid.minor = element_blank())

    } else {
      df <- city_index |>
        dplyr::filter(!is.na(sprawl_km2), !is.na(intens_km2))

      p <- ggplot(df,
                  aes(x      = intens_km2,
                      y      = sprawl_km2,
                      size   = delta_total_km2_2000_2025,
                      colour = subregion,
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
        scale_colour_viridis_d(option = "D", end = 0.9, name = "Sub-region") +
        labs(
          x     = "Densification — growth within existing footprint (km², sqrt scale)",
          y     = "New land — growth on previously unbuilt land (km², sqrt scale)",
          title = "Sprawl vs. intensification 2000–2025 — above diagonal = sprawl-dominant"
        ) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "right", panel.grid.minor = element_blank())
    }

    plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(legend = list(orientation = "v"))
  })
}

shinyApp(ui, server)
