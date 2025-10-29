################################################################################
# Maps for the app                                                             #
################################################################################
library(dplyr)
library(leaflet)
library(htmltools)
library(glue)

# Map definition---------------------------------------------------------------

#' Generates a symbol to indicate a bird sighting
#' @param order The bird order (e.g., Passeriformes, Accipitriformes)
#' @return icon
map_symbol <- function(order = "default") {
  # Map bird orders to their marker paths
  # Default to a generic bird icon if order-specific marker doesn't exist
  img <- case_when(
    order == "Passeriformes" ~ "bird-data/Markers/Passeriformes.svg",
    order == "Accipitriformes" ~ "bird-data/Markers/Accipitriformes.svg",
    order == "Anseriformes" ~ "bird-data/Markers/Anseriformes.svg",
    order == "Apodiformes" ~ "bird-data/Markers/Apodiformes.svg",
    order == "Caprimulgiformes" ~ "bird-data/Markers/Caprimulgiformes.svg",
    order == "Charadriiformes" ~ "bird-data/Markers/Charadriiformes.svg",
    order == "Pelecaniformes" ~ "bird-data/Markers/Pelecaniformes.svg",
    order == "Falconiformes" ~ "bird-data/Markers/Falconiformes.svg",
    order == "Columbiformes" ~ "bird-data/Markers/Columbiformes.svg",
    order == "Coraciiformes" ~ "bird-data/Markers/Coraciiformes.svg",
    order == "Cuculiformes" ~ "bird-data/Markers/Cuculiformes.svg",
    order == "Psittaciformes" ~ "bird-data/Markers/Psittaciformes.svg",
    order == "Gruiformes" ~ "bird-data/Markers/Gruiformes.svg",
    order == "Ciconiiformes" ~ "bird-data/Markers/Ciconiiformes.svg",
    order == "Galliformes" ~ "bird-data/Markers/Galliformes.svg",
    order == "Podicipediformes" ~ "bird-data/Markers/Podicipediformes.svg",
    order == "Strigiformes" ~ "bird-data/Markers/Strigiformes.svg",
    order == "marker" ~ "assets/marker.svg",
    order == "location" ~ "assets/location.svg",
    TRUE ~ "assets/marker.svg" # Use generic marker as fallback
  )
  size <- 24
  icon <- makeIcon(img, NULL, size, size, className = glue("marker {order}"))
  return(icon)
}

#' Get radar circle information for visualization
#' @param radius_range The filter radius range
#' @return list with radii and opacities
get_radar_info <- function(radius_range) {
  if (
    is.null(radius_range) ||
      length(radius_range) < 2 ||
      any(is.na(radius_range))
  ) {
    return(list(radii = numeric(0), opacities = numeric(0)))
  }

  radius_range <- as.numeric(radius_range)
  max_radius <- radius_range[2]
  radar_info <- case_when(
    max_radius == 0 ~ list(radii = 0, opacities = 0),
    max_radius <= 2.5 ~ list(radii = c(max_radius * 1000), opacities = c(0.15)),
    max_radius <= 5 ~ list(
      radii = c(max_radius * 500, max_radius * 1000),
      opacities = c(0.12, 0.08)
    ),
    max_radius <= 7.5 ~ list(
      radii = c(max_radius * 333, max_radius * 666, max_radius * 1000),
      opacities = c(0.15, 0.10, 0.07)
    ),
    max_radius <= 10 ~ list(
      radii = c(
        max_radius * 250,
        max_radius * 500,
        max_radius * 750,
        max_radius * 1000
      ),
      opacities = c(0.22, 0.13, 0.08, 0.05)
    ),
    TRUE ~ list(radii = 0, opacities = 0)
  )
  return(radar_info)
}

#' Handles leaflet rendering functions for the bird sighting map
#' @param map_data the dataset for bird sightings with spatial information
#' @param state the reactive "state" object
#' @return a leaflet widget
map_renderer <- function(map_data, state) {
  # Unpack state parameters
  center_lat <- state$center_lat
  center_lng <- state$center_lng
  zoom_level <- state$zoom_level
  radius_range <- state$filter_radius

  # Get radar info
  radar_info <- get_radar_info(radius_range)

  # Mapbox template location
  mapbox_template <- paste0(
    "https://api.mapbox.com/styles/v1/mapbox/outdoors-v12",
    "/tiles/{z}/{x}/{y}",
    "?access_token=",
    "pk.eyJ1IjoiaGs3NDAyIiwiYSI6ImNtaGJkM3BxdTB3bGQyaXB5czY2ZW1zMG0ifQ",
    ".yD4Rsrn1vPqxXk2AFgjOZA"
  )
  marker_orders <- ifelse(is.na(map_data$order), "default", map_data$order)
  unique_orders <- unique(marker_orders)
  icon_lookup <- setNames(lapply(unique_orders, map_symbol), unique_orders)
  marker_icons <- unname(icon_lookup[marker_orders])

  map <- map_data %>%
    # Initialise leaflet
    leaflet::leaflet(
      options = leaflet::leafletOptions(
        minZoom = 10,
        maxZoom = 18,
      ),
      sizingPolicy = leaflet::leafletSizingPolicy(
        defaultWidth = "100%",
        defaultHeight = "100%"
      )
    ) %>%
    # Add tile layer
    leaflet::addTiles(
      urlTemplate = mapbox_template,
      attribution = "© Mapbox © OpenStreetMap",
      options = leaflet::tileOptions(
        minZoom = 10,
        maxZoom = 18
      )
    ) %>%
    # Set initial view
    setView(lng = center_lng, lat = center_lat, zoom = zoom_level) %>%
    # Add Marker Layer with clustering
    leaflet::addMarkers(
      ~longitude,
      ~latitude,
      icon = marker_icons,
      clusterOptions = leaflet::markerClusterOptions(
        disableClusteringAtZoom = 15,
        spiderfyOnMaxZoom = TRUE,
        removeOutsideVisibleBounds = TRUE,
        maxClusterRadius = 80
      ),
      clusterId = "bird_clusters",
      popup = ~ paste0(
        "<div class='bird-popup'>",
        "<h3>",
        commonName,
        "</h3>",
        "<p><em>",
        scientificName,
        "</em></p>",
        "<p><strong>Order:</strong> ",
        order,
        "</p>",
        "<p><strong>Family:</strong> ",
        family,
        "</p>",
        "<p><strong>Count:</strong> ",
        count,
        "</p>",
        "<p><strong>Date:</strong> ",
        date,
        "</p>",
        "<p><strong>Rarity:</strong> ",
        rarityCategory,
        "</p>",
        "</div>"
      )
    ) %>%
    # Add Radar Circles showing search radius
    {
      if (length(radar_info$radii) > 0 && radar_info$radii[1] > 0) {
        # Add concentric circles for each radius
        map_with_circles <- .
        for (i in seq_along(radar_info$radii)) {
          map_with_circles <- map_with_circles %>%
            leaflet::addCircles(
              lat = center_lat,
              lng = center_lng,
              radius = radar_info$radii[i],
              color = "#7CB342", # Light green for bird theme
              fillColor = "#7CB342",
              fillOpacity = radar_info$opacities[i],
              weight = 0.2,
              stroke = TRUE
            )
        }
        map_with_circles
      } else {
        .
      }
    } %>%
    # Add Reference Scale
    leaflet::addScaleBar(
      position = "bottomleft",
      options = scaleBarOptions(
        metric = TRUE,
        imperial = FALSE
      )
    ) %>%
    # North arrow
    leaflet::addControl(
      html = htmltools::tags$img(
        width = 36,
        height = 36,
        src = "assets/north.svg"
      ),
      position = "bottomright",
      className = "leaflet-control-north-arrow"
    )

  return(map)
}
