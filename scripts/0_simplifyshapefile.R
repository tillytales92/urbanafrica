# Build a slim attribute table for the top-100 African agglomerations.
# Output: data/intermediate/agglom_attrs.Rds — used by the Shiny app.

pacman::p_load(tidyverse, here, sf, janitor)

agglom_top100 <- st_read(here("data/raw/africapolis/agglomerations.shp"),
                         quiet = TRUE) |>
  clean_names() |>
  slice_max(pop2020, n = 100)

agglom_top100 |>
  sf::st_drop_geometry() |>
  dplyr::transmute(
    slug       = janitor::make_clean_names(agglos_name),
    p_tree_cov,
    pop2020
  ) |>
  saveRDS(here("data/intermediate/agglom_attrs.Rds"))
