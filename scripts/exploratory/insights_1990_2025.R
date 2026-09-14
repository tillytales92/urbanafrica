library(here); setwd(here())
suppressMessages({library(dplyr); library(tidyr)})
options(width = 200)

bu  <- readRDS("data/intermediate/africapolis_builtup.Rds")      # id, agglosname, iso3, year, area_total_km2 ...
pop <- readRDS("data/intermediate/africapolis_pop.Rds")          # id, agglosname, iso3, year, pop
ci  <- readRDS("data/intermediate/city_index.Rds")               # slug, agglosname, iso3, total_km2_1990/2025, delta, lon/lat, shares...
sp  <- readRDS("data/intermediate/sprawl_stats.Rds")             # slug, sprawl_km2, intens_km2, sprawl_share, footprint_1990/2025_km2, density_1990/2025, density_change
ntl <- readRDS("data/intermediate/africapolis_ntl.Rds")          # id, agglosname, iso3, year, ntl_mean, ntl_lit_share, ntl_dimlit_share

reg <- function(iso3) {
  r <- countrycode::countrycode(iso3, "iso3c", "region23")
  dplyr::recode(r, "Northern Africa"="Northern","Western Africa"="West","Middle Africa"="Central",
                "Eastern Africa"="East","Southern Africa"="Southern")
}

# ---- built-up wide ----
buw <- bu |> select(agglosname, iso3, year, a = area_total_km2) |>
  pivot_wider(names_from = year, values_from = a, names_prefix = "y") |>
  mutate(region = reg(iso3),
         g_9025   = (y2025 - y1990)/y1990,
         g_early  = (y2005 - y1990)/y1990,      # 1990-2005
         g_late   = (y2025 - y2010)/y2010,      # 2010-2025
         add_9025 = y2025 - y1990,
         cagr_9025 = (y2025/y1990)^(1/35) - 1)

popw <- pop |> select(agglosname, iso3, year, p = pop) |>
  pivot_wider(names_from = year, values_from = p, names_prefix = "p") |>
  mutate(pg_9025 = (p2025 - p1990)/p1990)

d <- buw |> left_join(popw, by = c("agglosname","iso3")) |>
  mutate(landcap_1990 = y1990/(p1990/1e6),   # km2 built-up per million people
         landcap_2025 = y2025/(p2025/1e6),
         landcap_chg  = landcap_2025 - landcap_1990,
         bu_vs_pop    = g_9025 / pg_9025)     # >1 = footprint outran population

cat("=================  BUILT-UP GROWTH 1990->2025  =================\n")
cat("\n-- Top 10 by % growth --\n")
print(d |> arrange(desc(g_9025)) |> transmute(agglosname, region, `1990`=round(y1990), `2025`=round(y2025), `x`=round(y2025/y1990,1), `%`=round(g_9025*100)) |> head(10))
cat("\n-- Top 10 by absolute km2 added --\n")
print(d |> arrange(desc(add_9025)) |> transmute(agglosname, region, `1990`=round(y1990), `2025`=round(y2025), `+km2`=round(add_9025), `%`=round(g_9025*100)) |> head(10))
cat("\n-- Slowest 8 by % growth --\n")
print(d |> arrange(g_9025) |> transmute(agglosname, region, `1990`=round(y1990), `2025`=round(y2025), `%`=round(g_9025*100)) |> head(8))

cat("\n=================  ACCELERATION: early (1990-2005) vs late (2010-2025)  =================\n")
cat("(annualised-ish: comparing the two 15-yr windows' growth rates)\n")
cat("\n-- Biggest ACCELERATION (late rate >> early rate) --\n")
print(d |> filter(y1990 > 20) |> mutate(accel = g_late - g_early) |> arrange(desc(accel)) |>
      transmute(agglosname, region, `early%`=round(g_early*100), `late%`=round(g_late*100), `swing pp`=round((g_late-g_early)*100)) |> head(8))
cat("\n-- Biggest DECELERATION (early boom, late slowdown) --\n")
print(d |> filter(y1990 > 20) |> mutate(accel = g_late - g_early) |> arrange(accel) |>
      transmute(agglosname, region, `early%`=round(g_early*100), `late%`=round(g_late*100), `swing pp`=round((g_late-g_early)*100)) |> head(8))

cat("\n=================  REGIONAL  =================\n")
print(d |> group_by(region) |> summarise(n=n(),
        `med % growth`=round(median(g_9025)*100),
        `total km2 added`=round(sum(add_9025)),
        `med CAGR %`=round(median(cagr_9025)*100,1)) |> arrange(desc(`med % growth`)))

cat("\n=================  SPRAWL vs DENSIFICATION 1990->2025  =================\n")
spj <- sp |> left_join(ci |> select(slug, agglosname, iso3), by="slug") |> mutate(region=reg(iso3))
cat("\n-- Most sprawl-dominated (highest sprawl share, min 30 km2 growth) --\n")
print(spj |> filter(sprawl_km2+intens_km2 > 30) |> arrange(desc(sprawl_share)) |>
      transmute(agglosname, region, `new land km2`=round(sprawl_km2), `infill km2`=round(intens_km2), `sprawl %`=round(sprawl_share*100)) |> head(10))
cat("\n-- Most infill-dominated (lowest sprawl share, min 30 km2 growth) --\n")
print(spj |> filter(sprawl_km2+intens_km2 > 30) |> arrange(sprawl_share) |>
      transmute(agglosname, region, `new land km2`=round(sprawl_km2), `infill km2`=round(intens_km2), `sprawl %`=round(sprawl_share*100)) |> head(8))
cat("\n-- Density trajectory: biggest THINNING (footprint outran surface) --\n")
print(spj |> arrange(density_change) |> transmute(agglosname, region, d1990=round(density_1990,3), d2025=round(density_2025,3), chg=round(density_change,3)) |> head(8))
cat("\n-- biggest DENSIFYING --\n")
print(spj |> arrange(desc(density_change)) |> transmute(agglosname, region, d1990=round(density_1990,3), d2025=round(density_2025,3), chg=round(density_change,3)) |> head(8))

cat("\n=================  LAND vs PEOPLE 1990->2025  =================\n")
cat("bu_vs_pop = (built-up % growth) / (population % growth).  >1 => the city's footprint grew faster than its people.\n")
print(d |> filter(!is.na(bu_vs_pop), is.finite(bu_vs_pop)) |> arrange(desc(bu_vs_pop)) |>
      transmute(agglosname, region, `bu %`=round(g_9025*100), `pop %`=round(pg_9025*100), ratio=round(bu_vs_pop,2)) |> head(10))
cat("\n-- overall --\n")
cat(sprintf("cities where footprint outran population (ratio>1): %d of %d\n", sum(d$bu_vs_pop>1, na.rm=TRUE), sum(is.finite(d$bu_vs_pop))))
cat(sprintf("median built-up per-capita land 1990: %.0f  -> 2025: %.0f km2 per million residents\n",
            median(d$landcap_1990, na.rm=TRUE), median(d$landcap_2025, na.rm=TRUE)))
cat(sprintf("continent total built-up (top-100): %.0f km2 (1990)  ->  %.0f km2 (2025)   (+%.0f%%)\n",
            sum(d$y1990), sum(d$y2025), (sum(d$y2025)/sum(d$y1990)-1)*100))
cat(sprintf("continent total population (top-100): %.1f M (1990)  ->  %.1f M (2025)   (+%.0f%%)\n",
            sum(d$p1990)/1e6, sum(d$p2025)/1e6, (sum(d$p2025)/sum(d$p1990)-1)*100))

cat("\n=================  NIGHTTIME LIGHTS (corrected DMSP)  =================\n")
nn <- ntl |> filter(year %in% c(1992,2000,2025)) |> mutate(dark_dim = (1-ntl_lit_share)+ntl_dimlit_share) |>
  select(agglosname, iso3, year, dark_dim) |> pivot_wider(names_from=year, values_from=dark_dim, names_prefix="dd") |>
  mutate(region=reg(iso3), drop = dd2000 - dd2025)
cat("\n-- Biggest fall in dark-or-dim share, 2000->2025 (electrification/intensification of lighting) --\n")
print(nn |> arrange(desc(drop)) |> transmute(agglosname, region, `2000 %`=round(dd2000*100), `2025 %`=round(dd2025*100), `drop pp`=round(drop*100)) |> head(10))
cat("\n-- Still most under-lit in 2025 --\n")
print(nn |> arrange(desc(dd2025)) |> transmute(agglosname, region, `2025 dark/dim %`=round(dd2025*100)) |> head(10))
