# 2_4_sprawl_metrics.R
# Computes sprawl vs intensification metrics for all cities.
#
# Decomposition (full period 2000->2025, using total built-up surface):
#   Sprawl (extensive margin) : pixels that were 0 in 2000 and >0 in 2025
#   Intensification (intensive): pixels that were >0 in 2000 and increased
#   Shrinkage                  : pixels that decreased (usually negligible)
#
# All areas in km². Pixel area = 100m x 100m = 0.01 km².
# Uses native Mollweide stacks (total/res/nres .tif) -- equal-area CRS
# so summing pixel values and dividing by 1e6 gives km^2 of built-up surface.
#
# Also computes a density index: built-up km^2 / footprint km^2 for 2000 and
# 2025. A falling density index means the city is consuming land faster than
# it is adding built-up surface -- the built form is spreading thin (sprawl).
#
# Outputs:
#   data/intermediate/sprawl_stats.Rds
#   app/data/sprawl_stats.Rds

pacman::p_load(terra, dplyr, here)

cities_dir <- here("data/intermediate/cities")
PIXEL_KM2  <- 0.01   # 100 m x 100 m

margins <- function(r0, r1) {
  list(
    sprawl = sum(values(ifel(r0 == 0 & r1 > 0, r1,      0)), na.rm = TRUE) / 1e6,
    intens = sum(values(ifel(r0 > 0 & r1 > r0, r1 - r0, 0)), na.rm = TRUE) / 1e6,
    shrink = sum(values(ifel(r1 < r0,           r0 - r1, 0)), na.rm = TRUE) / 1e6
  )
}

compute_city <- function(slug) {
  d <- file.path(cities_dir, slug)
  if (!dir.exists(d)) return(NULL)

  tryCatch({
    tot  <- rast(file.path(d, "total.tif"))
    res  <- rast(file.path(d, "res.tif"))
    nres <- rast(file.path(d, "nres.tif"))

    m_tot  <- margins(tot[["2000"]],  tot[["2025"]])
    m_res  <- margins(res[["2000"]],  res[["2025"]])
    m_nres <- margins(nres[["2000"]], nres[["2025"]])

    # Footprint = number of pixels with any built-up surface
    fp2000 <- sum(values(tot[["2000"]]) > 0, na.rm = TRUE) * PIXEL_KM2
    fp2025 <- sum(values(tot[["2025"]]) > 0, na.rm = TRUE) * PIXEL_KM2

    bu2000 <- sum(values(tot[["2000"]]), na.rm = TRUE) / 1e6
    bu2025 <- sum(values(tot[["2025"]]), na.rm = TRUE) / 1e6

    tot_change <- m_tot$sprawl + m_tot$intens

    tibble(
      slug               = slug,
      # Total built-up margins
      sprawl_km2         = m_tot$sprawl,
      intens_km2         = m_tot$intens,
      shrink_km2         = m_tot$shrink,
      sprawl_share       = if (tot_change > 0) m_tot$sprawl / tot_change else NA_real_,
      # Residential margins
      sprawl_res_km2     = m_res$sprawl,
      intens_res_km2     = m_res$intens,
      # Non-residential margins
      sprawl_nres_km2    = m_nres$sprawl,
      intens_nres_km2    = m_nres$intens,
      # Footprint and density trajectory
      footprint_2000_km2 = fp2000,
      footprint_2025_km2 = fp2025,
      density_2000       = if (fp2000 > 0) bu2000 / fp2000 else NA_real_,
      density_2025       = if (fp2025 > 0) bu2025 / fp2025 else NA_real_,
      density_change     = if (fp2000 > 0 & fp2025 > 0) (bu2025 / fp2025) - (bu2000 / fp2000) else NA_real_
    )
  }, error = function(e) {
    message("  FAILED: ", slug, " -- ", conditionMessage(e))
    NULL
  })
}

cities <- list.dirs(cities_dir, recursive = FALSE, full.names = FALSE)
message("Computing sprawl metrics for ", length(cities), " cities...")

sprawl_stats <- bind_rows(lapply(cities, function(slug) {
  message("  ", slug)
  compute_city(slug)
}))

message("Writing outputs...")
saveRDS(sprawl_stats, here("data/intermediate/sprawl_stats.Rds"))
saveRDS(sprawl_stats, here("app/data/sprawl_stats.Rds"))
message("Done. ", nrow(sprawl_stats), " cities.")
print(summary(sprawl_stats[, c("sprawl_km2", "intens_km2", "sprawl_share",
                                "density_2000", "density_2025", "density_change")]))
