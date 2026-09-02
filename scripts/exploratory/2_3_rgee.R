# 2_3_rgee.R
# African capital urban growth -- Landsat false-colour composites
# SWIR1-NIR-Red: green = vegetation, magenta = urban, gold = new built-up

# -- 1. Environment -----------------------------------------------------------

# RGEE_PYTHON = path to the Python/conda env with `earthengine-api`
# (set in ~/.Renviron). Unset -> reticulate picks its default.
rgee_env_dir <- Sys.getenv("RGEE_PYTHON")
if (nzchar(rgee_env_dir)) {
  Sys.setenv(RETICULATE_PYTHON  = rgee_env_dir)
  Sys.setenv(EARTHENGINE_PYTHON = rgee_env_dir)
}

library(reticulate)

pacman::p_load(rgee, googledrive, leaflet, tidyverse, here)

# -- 2. Authenticate ----------------------------------------------------------

rgee::ee_Authenticate()
rgee::ee_Initialize(drive = TRUE)
ee_check()
googledrive::drive_auth()

# -- 3. African Capitals Bounding Boxes ---------------------------------------

# Boxes are sized to the metropolitan footprint of each city, not just the
# city-proper boundary. Coordinates in WGS84 (xmin, ymin, xmax, ymax).
african_capitals <- tribble(
  ~country,                    ~capital,          ~xmin,   ~ymin,   ~xmax,   ~ymax,
  # North Africa
  "Algeria",                   "Algiers",          2.85,   36.65,    3.25,   36.95,
  "Egypt",                     "Cairo",           30.85,   29.80,   31.70,   30.25,
  "Libya",                     "Tripoli",         13.10,   32.80,   13.35,   33.00,
  "Morocco",                   "Rabat",           -6.95,   33.93,   -6.72,   34.05,
  "Sudan",                     "Khartoum",        32.40,   15.50,   32.80,   15.75,
  "Tunisia",                   "Tunis",           10.05,   36.72,   10.30,   36.90,
  # West Africa
  "Benin",                     "Porto-Novo",       2.58,    6.35,    2.75,    6.50,
  "Burkina Faso",              "Ouagadougou",     -1.65,   12.30,   -1.40,   12.50,
  "Cabo Verde",                "Praia",          -23.60,   14.88,  -23.48,   14.98,
  "Cote d'Ivoire",             "Yamoussoukro",    -5.35,    6.78,   -5.18,    6.92,
  "Gambia",                    "Banjul",         -16.70,   13.40,  -16.55,   13.50,
  "Ghana",                     "Accra",           -0.35,    5.42,    0.10,    5.75,
  "Guinea",                    "Conakry",        -13.82,    9.50,  -13.55,    9.70,
  "Guinea-Bissau",             "Bissau",         -15.70,   11.78,  -15.52,   11.90,
  "Liberia",                   "Monrovia",       -10.90,    6.25,  -10.72,    6.42,
  "Mali",                      "Bamako",          -8.10,   12.55,   -7.90,   12.75,
  "Mauritania",                "Nouakchott",     -15.97,   17.95,  -15.72,   18.12,
  "Niger",                     "Niamey",           2.00,   13.40,    2.22,   13.62,
  "Nigeria",                   "Abuja",            7.20,    8.80,    7.65,    9.18,
  "Senegal",                   "Dakar",          -17.55,   14.62,  -17.32,   14.80,
  "Sierra Leone",              "Freetown",       -13.35,    8.40,  -13.10,    8.55,
  "Togo",                      "Lome",             1.12,    6.08,    1.32,    6.28,
  # Central Africa
  "Cameroon",                  "Yaounde",         11.42,    3.78,   11.65,    3.98,
  "Central African Republic",  "Bangui",          18.50,    4.32,   18.66,    4.45,
  "Chad",                      "N'Djamena",       14.95,   12.02,   15.16,   12.22,
  "DR Congo",                  "Kinshasa",        15.18,   -4.50,   15.52,   -4.18,
  "Equatorial Guinea",         "Malabo",           8.70,    3.70,    8.82,    3.82,
  "Gabon",                     "Libreville",       9.38,    0.32,    9.52,    0.48,
  "Republic of Congo",         "Brazzaville",     15.20,   -4.40,   15.40,   -4.22,
  "Sao Tome and Principe",     "Sao Tome",         6.70,    0.30,    6.80,    0.40,
  # East Africa
  "Burundi",                   "Gitega",          29.90,   -3.50,   30.05,   -3.40,
  "Comoros",                   "Moroni",          43.20,  -11.75,   43.30,  -11.65,
  "Djibouti",                  "Djibouti City",   43.10,   11.52,   43.22,   11.62,
  "Eritrea",                   "Asmara",          38.90,   15.32,   39.05,   15.42,
  "Ethiopia",                  "Addis Ababa",     38.60,    8.85,   38.90,    9.15,
  "Kenya",                     "Nairobi",         36.60,   -1.42,   37.10,   -1.12,
  "Madagascar",                "Antananarivo",    47.45,  -19.02,   47.66,  -18.85,
  "Malawi",                    "Lilongwe",        33.72,  -14.05,   33.95,  -13.88,
  "Mauritius",                 "Port Louis",      57.44,  -20.20,   57.56,  -20.10,
  "Mozambique",                "Maputo",          32.50,  -25.97,   32.72,  -25.80,
  "Rwanda",                    "Kigali",          30.00,   -1.98,   30.22,   -1.85,
  "Seychelles",                "Victoria",        55.44,   -4.70,   55.54,   -4.62,
  "Somalia",                   "Mogadishu",       45.27,    2.02,   45.48,    2.12,
  "South Sudan",               "Juba",            31.52,    4.82,   31.68,    4.92,
  "Tanzania",                  "Dodoma",          35.70,   -6.26,   35.92,   -6.10,
  "Uganda",                    "Kampala",         32.50,    0.22,   32.76,    0.45,
  "Zambia",                    "Lusaka",          28.22,  -15.52,   28.52,  -15.22,
  "Zimbabwe",                  "Harare",          31.00,  -17.90,   31.25,  -17.72,
  # Southern Africa
  "Angola",                    "Luanda",          13.22,   -8.95,   13.52,   -8.70,
  "Botswana",                  "Gaborone",        25.82,  -24.72,   26.02,  -24.60,
  "Eswatini",                  "Mbabane",         31.10,  -26.45,   31.22,  -26.30,
  "Lesotho",                   "Maseru",          27.42,  -29.40,   27.55,  -29.28,
  "Namibia",                   "Windhoek",        16.95,  -22.65,   17.18,  -22.50,
  "South Africa",              "Pretoria",        28.10,  -25.85,   28.38,  -25.68
)

# -- 4. Study Area Function ---------------------------------------------------

# Look up a capital by name, create an EE geometry, and centre the map.
# city     : capital name (case-insensitive; partial matches accepted)
# zoom     : EE map zoom level; auto-computed from bbox width if NULL
# capitals : the dataframe to look up from (defaults to african_capitals)
set_study_area <- function(city, zoom = NULL, capitals = african_capitals) {
  row <- capitals |> filter(str_to_lower(capital) == str_to_lower(city))

  # Fall back to partial match if exact match fails
  if (nrow(row) == 0) {
    row <- capitals |>
      filter(str_detect(str_to_lower(capital), str_to_lower(city)))
  }

  if (nrow(row) == 0) {
    stop(
      "Capital '", city, "' not found.\nAvailable cities:\n  ",
      paste(sort(capitals$capital), collapse = ", ")
    )
  }
  if (nrow(row) > 1) {
    message("Multiple matches -- using: ", row$capital[1])
    row <- row[1, ]
  }

  # Scale zoom to bbox width so small cities are not zoomed out too far
  if (is.null(zoom)) {
    w    <- row$xmax - row$xmin
    zoom <- dplyr::case_when(w < 0.20 ~ 13, w < 0.35 ~ 12,
                             w < 0.55 ~ 11, w < 0.80 ~ 10, TRUE ~ 9)
  }

  cat(sprintf("Study area: %s, %s (zoom %d)\n", row$capital, row$country, zoom))

  bbox <- ee$Geometry$Rectangle(
    coords   = c(row$xmin, row$ymin, row$xmax, row$ymax),
    proj     = "EPSG:4326",
    geodesic = FALSE
  )

  Map$centerObject(bbox, zoom)
  Map$addLayer(bbox, list(color = "red"), paste(row$capital, "boundary"))

  bbox
}

# -- 5. Set Study City --------------------------------------------------------

# Change the city name here to switch to any African capital
city <- set_study_area("Antananarivo")

# -- 6. Cloud Masking + Full-Coverage Median Composites -----------------------

mask_clouds <- function(image) {
  qa <- image$select("QA_PIXEL")
  image$updateMask(
    qa$bitwiseAnd(8L)$eq(0L)$And(   # no cloud shadow (bit 3)
    qa$bitwiseAnd(16L)$eq(0L))      # no cloud (bit 4)
  )
}

# .median() across all intersecting scenes guarantees full bbox coverage.
# .clip() constrains output to the exact bounding box.
# Merges Landsat 8 + 9 for maximum scene density (L9 operational from 2022).
make_composite <- function(year, bounds) {
  l8 <- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")
  l9 <- ee$ImageCollection("LANDSAT/LC09/C02/T1_L2")

  l8$merge(l9)$
    filterBounds(bounds)$
    filterDate(paste0(year, "-01-01"), paste0(year, "-12-31"))$
    filter(ee$Filter$lt("CLOUD_COVER", 30))$
    map(mask_clouds)$
    select(
      c("SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7"),
      c("blue",  "green", "red",   "nir",   "swir1", "swir2")
    )$
    median()$
    clip(bounds)
}

image2015 <- make_composite(2015, city)
image2025 <- make_composite(2025, city)

# -- 7. Visualisation: SWIR1-NIR-Red False Colour -----------------------------
#
#   Bright green    -> dense vegetation   (NIR high -> bright G channel)
#   Magenta / pink  -> urban / impervious (SWIR1 high -> bright R channel)
#   Brown / tan     -> bare soil or sparse cover
#   Dark blue-black -> water
#
vis <- list(
  bands = c("swir1", "nir", "red"),
  min   = 7500,
  max   = 22000,
  gamma = 1.2
)

# -- 8. Side-by-Side View with New Built-Up Overlay ---------------------------

map_2015 <- Map$addLayer(image2015, vis, "Landsat 2015")
map_2025 <- Map$addLayer(image2025, vis, "Landsat 2025")

# Scale C2 L2 DN to true reflectance before index computation
apply_sr_scale <- function(image) image$multiply(0.0000275)$add(-0.2)

# Built-up: NDBI > 0 (impervious/bare soil) AND NDVI < 0.2 (non-vegetated).
# The NDVI guard removes bare agricultural fields that would otherwise trigger
# as false positives on NDBI alone.
classify_builtup <- function(image) {
  sr   <- apply_sr_scale(image)
  ndbi <- sr$normalizedDifference(c("swir1", "nir"))
  ndvi <- sr$normalizedDifference(c("nir",   "red"))
  ndbi$gt(0.0)$And(ndvi$lt(0.2))
}

builtup_2015 <- classify_builtup(image2015)
builtup_2025 <- classify_builtup(image2025)

# Pixels that flipped from non-built-up to built-up between the two years
new_builtup <- builtup_2025$And(builtup_2015$Not())

# selfMask() makes 0-valued pixels transparent; only new-development pixels render.
# Gold contrasts with both the green (vegetation) and magenta (urban) tones.
map_new_builtup <- Map$addLayer(
  new_builtup$selfMask(),
  list(palette = "red", min = 1, max = 1),
  "New built-up 2015-2025",
  opacity = 0.8
)

# Left: 2015 baseline  |  Right: 2025 + new development in gold
map_2015 | (map_2025 + map_new_builtup)
