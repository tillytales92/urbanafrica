# One-off export for the scrollytelling bubble-chart artifact (urban growth,
# 1990-2025). Not part of the numbered pipeline. Writes a compact JSON of
# per-city/per-epoch built-up + population + region + sprawl stats, used to
# build output/urban_growth_story.html (D3, fetched at runtime, no server).
pacman::p_load(here, dplyr, tidyr, countrycode, jsonlite)

bu  <- readRDS(here("data/intermediate/africapolis_builtup.Rds"))
pop <- readRDS(here("data/intermediate/africapolis_pop.Rds"))
ci  <- readRDS(here("data/intermediate/city_index.Rds"))
sp  <- readRDS(here("data/intermediate/sprawl_stats.Rds"))

region_levels <- c("Northern Africa", "West Africa", "Central Africa",
                    "East Africa", "Southern Africa")
reg <- function(iso3) {
  r <- countrycode::countrycode(iso3, "iso3c", "region23")
  recode(r, "Northern Africa" = "Northern Africa", "Western Africa" = "West Africa",
         "Middle Africa" = "Central Africa", "Eastern Africa" = "East Africa",
         "Southern Africa" = "Southern Africa")
}

ci2 <- ci |>
  mutate(macro_region = reg(iso3), pct_growth = delta_total_km2_1990_2025 / total_km2_1990) |>
  left_join(sp |> select(slug, sprawl_share), by = "slug")

panel <- bu |>
  select(agglosname, iso3, year, area_total_km2) |>
  inner_join(pop |> select(agglosname, iso3, year, pop), by = c("agglosname", "iso3", "year")) |>
  inner_join(ci2 |> select(slug, agglosname, iso3, macro_region, sprawl_share, pct_growth),
             by = c("agglosname", "iso3")) |>
  transmute(slug, name = agglosname, iso3, region = macro_region,
            year, area_km2 = round(area_total_km2, 2),
            pop = round(pop), density = round(pop / area_total_km2, 1),
            sprawl_share = round(sprawl_share, 3), pct_growth = round(pct_growth, 3)) |>
  arrange(slug, year)

highlight <- tibble::tibble(
  slug  = c("lagos", "johannesburg", "kinshasa", "al_qahira", "nairobi",
            "kigali", "abuja", "hawassa", "tunis", "juba"),
  story = c(
    "Sprawl-dominant megacity, still accelerating onto greenfield land.",
    "Infill-dominant megacity: its 2025 edge is close to its 1990 edge.",
    "Huge population growth almost entirely absorbed inside a near-static footprint.",
    "Biggest absolute land gain of any city in the sample, steady growth.",
    "The engine of the fastest-growing macro-region, East Africa.",
    "A small secondary city compounding fast off a low 1990 base.",
    "Built as a new capital from 1990 — explosive early growth, then decelerating.",
    "The sprawl frontier: fast growth, thinning density, from a tiny 1990 base.",
    "The compact exception — growth almost entirely by filling in, not spreading.",
    "Grew +197% after 2011 independence while density rose, not fell."
  )
)

stopifnot(all(highlight$slug %in% panel$slug))

out <- list(
  cities = panel,
  highlight = highlight,
  years = sort(unique(panel$year))
)

write_json(out, here("output/urban_growth_story_data.json"),
           auto_unbox = TRUE, digits = 4)

# Bake the JSON directly into urban_growth_story.html between the
# <!--DATA:START-->/<!--DATA:END--> markers. Opening the .html via file://
# (double-click) blocks fetch() of a sibling .json -- only a hosted page
# (e.g. the Claude artifact) can fetch it -- so the shipped file must be
# self-contained. The .json stays alongside it for inspection/reuse.
html_path <- here("output/urban_growth_story.html")
html_txt  <- paste(readLines(html_path, warn = FALSE), collapse = "\n")
json_txt  <- paste(readLines(here("output/urban_growth_story_data.json"), warn = FALSE), collapse = "")
baked <- sub("(?s)<!--DATA:START-->.*?<!--DATA:END-->",
              paste0("<!--DATA:START-->", json_txt, "<!--DATA:END-->"),
              html_txt, perl = TRUE, fixed = FALSE)
if (identical(baked, html_txt)) stop("DATA:START/END markers not found in ", html_path)
writeLines(baked, html_path)

cat("Wrote", nrow(panel), "rows,", length(unique(panel$slug)), "cities,",
    length(unique(panel$year)), "epochs. Baked into", html_path, "\n")
