################################################################################
# Data handlers for the app                                                    #
################################################################################
library(dplyr)
library(readr)
library(glue)
library(lubridate)

#' Calculate haversine distance (in kilometres) between vectors of coordinates
#' @param lat numeric vector of latitudes
#' @param lon numeric vector of longitudes
#' @param center_lat single numeric latitude (degrees)
#' @param center_lon single numeric longitude (degrees)
#' @return numeric vector of distances in kilometres
haversine_km <- function(lat, lon, center_lat, center_lon) {
  if (length(lat) == 0) {
    return(numeric(0))
  }

  # Convert degrees to radians
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

  return(distance)
}

#' Load bird sighting data from CSV
#' @param filename the filename for the CSV file to be loaded
#' @return dataframe
load_bird_data <- function(filename = "Data/Pre - Processed Data/data.csv") {
  print(glue("Loading bird data from {filename}"))

  # Read the CSV file
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
    filter(year >= 1985, year <= 2019)

  print(glue("Loaded dataframe with {nrow(df)} rows and {ncol(df)} columns"))
  print(glue("Unique species: {n_distinct(df$scientificName)}"))
  print(glue("Year range: {min(df$year, na.rm = TRUE)} - {max(df$year, na.rm = TRUE)}"))

  return(df)
}

#' Get list of unique species from the data
#' @param data The bird sighting dataframe
#' @return vector of unique species names
get_species_list <- function(data) {
  species <- data %>%
    distinct(scientificName, commonName) %>%
    arrange(commonName) %>%
    pull(commonName)

  return(species)
}

#' Get list of unique bird orders from the data
#' @param data The bird sighting dataframe
#' @return vector of unique orders
get_order_list <- function(data) {
  orders <- data %>%
    distinct(order) %>%
    arrange(order) %>%
    pull(order)

  return(orders)
}

#' Filter bird data based on various criteria
#' @param data The bird sighting dataframe
#' @param species Filter by species (optional)
#' @param order Filter by taxonomic order (optional)
#' @param year_range Filter by year range (optional)
#' @param radius_range Filter by distance range in km (optional)
#' @param center_location Center point for radius filter c(lat, lng) (optional)
#' @param rarity_filters List of rarity categories to show (optional)
#' @return filtered dataframe
filter_bird_data <- function(data,
                              species = NULL,
                              order = NULL,
                              year_range = NULL,
                              radius_range = NULL,
                              center_location = NULL,
                              rarity_filters = NULL) {
  filtered <- data

  # Filter by species if provided
  if (!is.null(species) && length(species) > 0) {
    filtered <- filtered %>%
      filter(commonName %in% species)
  }

  # Filter by order if provided
  if (!is.null(order) && length(order) > 0) {
    filtered <- filtered %>%
      filter(.data$order %in% order)
  }

  # Filter by rarity if provided
  if (!is.null(rarity_filters) && length(rarity_filters) > 0) {
    filtered <- filtered %>%
      filter(rarityCategory %in% rarity_filters)
  }

  # Filter by year range if provided
  if (!is.null(year_range)) {
    filtered <- filtered %>%
      filter(year >= year_range[1] & year <= year_range[2])
  }

  # Filter by distance if both radius and center location provided
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
        distance >= radius_range[1],
        distance <= radius_range[2]
      )
  }

  # Deduplicate bird species within proximity to prevent multiple instances in spirals
  # At zoom levels 10-14: Birds cluster and form spirals - we want only unique species
  # At zoom level 15+: Clustering is disabled, all birds show at actual locations

  if (nrow(filtered) > 0) {
    # Sort to prioritize best observations: most recent date, highest count
    filtered <- filtered %>%
      arrange(scientificName, desc(date), desc(count))

    # Track which rows to keep
    keep_rows <- rep(TRUE, nrow(filtered))

    # For each bird, check if there are duplicate species nearby
    for (i in seq_len(nrow(filtered))) {
      if (!keep_rows[i]) next  # Already marked as duplicate

      # Find later observations of the same species
      same_species_later <- which(
        filtered$scientificName == filtered$scientificName[i] &
        keep_rows &
        seq_len(nrow(filtered)) > i
      )

      if (length(same_species_later) > 0) {
        # Calculate distances to other same-species observations
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

    # Keep only non-duplicate observations
    filtered <- filtered[keep_rows, ]
  }

  # Add unique row ID for marker identification (needed for modal dialog clicks)
  filtered <- filtered %>%
    mutate(marker_id = row_number())

  return(filtered)
}
