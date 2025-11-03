library(dplyr)
library(readr)
library(lubridate)
library(rlang)

utils::globalVariables(c(
  "scientificName",
  "commonName",
  "order",
  "rarityCategory",
  "distance",
  "date",
  "count",
  "marker_id"
))

# Haversine distance function (vectorised)
# Inspired by: https://github.com/johnsonjzhou/geom90007-a3
# Additional mathematical formulation based on:
# https://www.movable-type.co.uk/scripts/latlong.html
haversine_km <- function(lat, lon, center_lat, center_lon) {
  if (length(lat) == 0) {
    return(numeric(0))
  }

  rad <- pi / 180
  lat_rad <- lat * rad
  lon_rad <- lon * rad
  center_lat_rad <- center_lat * rad
  center_lon_rad <- center_lon * rad

  # Haversine formula (vectorised)
  delta_lat <- lat_rad - center_lat_rad
  delta_lon <- lon_rad - center_lon_rad

  a <- sin(delta_lat / 2)^2 +
    cos(center_lat_rad) * cos(lat_rad) * sin(delta_lon / 2)^2

  c <- 2 * asin(pmin(1, sqrt(a)))
  earth_radius_km <- 6371.0088
  distance <- earth_radius_km * c

  distance
}

load_bird_data <- function(filename = "Data/Pre - Processed Data/data.csv") {
  df <- readr::read_csv(
    filename,
    col_types = cols(
      scientificName = col_character(),
      commonName = col_character(),
      order = col_character(),
      family = col_character(),
      genus = col_character(),
      latitude = col_double(),
      longitude = col_double(),
      date = col_datetime(format = ""),
      count = col_integer(),
      rarityCategory = col_character(),
      description = col_character(),
      imagePath = col_character(),
      markerPath = col_character(),
      audioPath = col_character(),
      creditsPath = col_character()
    )
  ) %>%
    # Extract year from date for filtering
    mutate(year = lubridate::year(date)) %>%
    filter(year >= 1998, year <= 2019)

  image_folder <- df$imagePath
  image_file <- file.path(image_folder, paste0(basename(image_folder), ".jpg"))
  image_src <- file.path("bird-data", sub("^Data/", "", image_file))

  audio_file <- if_else(
    df$audioPath == "No Audio Data",
    NA_character_,
    df$audioPath
  )
  audio_src <- if_else(
    is.na(audio_file),
    NA_character_,
    file.path("bird-data", sub("^Data/", "", audio_file))
  )

  credits_file <- if_else(
    df$creditsPath == "No Audio Data",
    NA_character_,
    df$creditsPath
  )
  credits_src <- if_else(
    is.na(credits_file),
    NA_character_,
    file.path("bird-data", sub("^Data/", "", credits_file))
  )

  df %>%
    mutate(
      markerPath = file.path(
        "bird-data/Markers",
        paste0(.data$order, "_", .data$rarityCategory, ".svg")
      ),
      image_file = image_file,
      image_src = image_src,
      audio_file = audio_file,
      audio_src = audio_src,
      credits_file = credits_file,
      credits_src = credits_src
    ) %>%
    select(any_of(c(
      "scientificName",
      "commonName",
      "order",
      "family",
      "genus",
      "latitude",
      "longitude",
      "date",
      "count",
      "year",
      "rarityCategory",
      "description",
      "image_file",
      "image_src",
      "audio_file",
      "audio_src",
      "credits_file",
      "credits_src",
      "markerPath"
    )))
}

filter_bird_data <- function(
  data,
  species = NULL,
  order = NULL,
  year_range = NULL,
  radius_range = NULL,
  center_location = NULL,
  rarity_filters = NULL
) {
  filtered <- data

  if (!is.null(species) && length(species) > 0) {
    filtered <- filtered %>%
      filter(.data$commonName %in% species)
  }

  if (!is.null(order) && length(order) > 0) {
    filtered <- filtered %>%
      filter(.data$order %in% order)
  }

  if (!is.null(rarity_filters) && length(rarity_filters) > 0) {
    filtered <- filtered %>%
      filter(.data$rarityCategory %in% rarity_filters)
  }

  if (!is.null(year_range)) {
    filtered <- filtered %>%
      filter(.data$year >= year_range[1] & .data$year <= year_range[2])
  }

  if (!is.null(radius_range) && !is.null(center_location)) {
    center_lat <- center_location[1]
    center_lon <- center_location[2]

    distances <- haversine_km(
      lat = filtered$latitude,
      lon = filtered$longitude,
      center_lat = center_lat,
      center_lon = center_lon
    )

    filtered <- filtered %>%
      mutate(distance = distances) %>%
      filter(
        .data$distance >= radius_range[1],
        .data$distance <= radius_range[2]
      )
  }

  # Deduplicate nearby observations to avoid spiral duplicates in clusters.
  # Zoom 10-14 keeps one per species; zoom 15+ shows all observations.

  if (nrow(filtered) > 0) {
    # Sort to prioritize best observations: most recent date, highest count
    filtered <- filtered %>%
      arrange(.data$scientificName, desc(.data$date), desc(.data$count))

    keep_rows <- rep(TRUE, nrow(filtered))

    for (i in seq_len(nrow(filtered))) {
      if (!keep_rows[i]) {
        next
      }

      same_species_later <- which(
        filtered$scientificName == filtered$scientificName[i] &
          keep_rows &
          seq_len(nrow(filtered)) > i
      )

      if (length(same_species_later) > 0) {
        distances <- haversine_km(
          lat = filtered$latitude[same_species_later],
          lon = filtered$longitude[same_species_later],
          center_lat = filtered$latitude[i],
          center_lon = filtered$longitude[i]
        )

        # Mark duplicates within 1km as duplicates (prevents spiral confusion)
        # This distance matches the clustering radius at zoom levels 13-14
        # where spirals most commonly occur (80 pixels ≈ 0.75-2 km)
        duplicates <- same_species_later[distances <= 1.0]
        keep_rows[duplicates] <- FALSE
      }
    }

    filtered <- filtered[keep_rows, ]
  }

  # Add unique row ID for marker identification (needed for modal dialog clicks)
  filtered <- filtered %>%
    mutate(marker_id = row_number())

  filtered
}
