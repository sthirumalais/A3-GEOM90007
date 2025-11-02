################################################################################
# Maps for the app                                                             #
################################################################################
library(dplyr)
library(leaflet)
library(htmltools)
library(glue)
library(base64enc)

# Map definition---------------------------------------------------------------

#' Gets the icon path for a bird sighting based on taxonomic order
#' @param order The bird order (e.g., Passeriformes, Accipitriformes)
#' @return character path to icon
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
  return(img)
}

#' Translate a web-accessible asset path to its filesystem counterpart
#' @param web_path character path used in the Shiny app
#' @return character path on disk
resolve_asset_path <- function(web_path) {
  if (startsWith(web_path, "bird-data/")) {
    file.path("Data", sub("^bird-data/", "", web_path))
  } else if (startsWith(web_path, "assets/")) {
    file.path("www", sub("^assets/", "", web_path))
  } else {
    web_path
  }
}

#' Retrieve an SVG as a data URI, with simple in-memory caching
#' @param web_path character path used in the app
#' @return data URI string or NULL if missing
get_svg_data_uri <- local({
  cache <- new.env(parent = emptyenv())
  function(web_path) {
    if (!exists(web_path, envir = cache, inherits = FALSE)) {
      file_path <- resolve_asset_path(web_path)
      if (!file.exists(file_path)) {
        cache[[web_path]] <- NULL
      } else {
        svg_text <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
        cache[[web_path]] <- base64enc::dataURI(
          data = charToRaw(svg_text),
          mime = "image/svg+xml"
        )
      }
    }
    cache[[web_path]]
  }
})

#' Gets the rarity marker background path for a bird sighting
#' @param rarity The rarity label (e.g., Common, Rare)
#' @return character path to marker background
rarity_symbol <- function(rarity = "default") {
  case_when(
    rarity == "Common" ~ "bird-data/Markers/markerGreen.svg",
    rarity == "Fairly Common" ~ "bird-data/Markers/markerYellow.svg",
    rarity == "Uncommon" ~ "bird-data/Markers/markerOrange.svg",
    rarity == "Rare" ~ "bird-data/Markers/markerRed.svg",
    rarity == "Vagrant" ~ "bird-data/Markers/markerViolet.svg",
    TRUE ~ "assets/marker.svg"
  )
}

#' Create leaflet icons using pre-generated overlay icons
#' @param orders Character vector of taxonomic orders
#' @param rarities Character vector of rarity categories
#' @return leaflet icon set
compose_marker_icons <- function(orders, rarities) {
  if (length(orders) == 0) {
    return(icons(iconUrl = character(0)))
  }

  clean_orders <- ifelse(is.na(orders) | orders == "", "default", orders)
  clean_rarities <- ifelse(is.na(rarities) | rarities == "", "default", rarities)

  normalise_web_path <- function(path) {
    # Replace spaces to keep URLs safe while preserving directory separators
    gsub(" ", "%20", path, fixed = TRUE)
  }

  marker_urls <- vapply(
    seq_along(clean_orders),
    FUN = function(idx) {
      order <- clean_orders[[idx]]
      rarity <- clean_rarities[[idx]]

      candidate_path <- paste0("bird-data/Markers_new/", order, "_", rarity, ".svg")
      candidate_file <- resolve_asset_path(candidate_path)

      if (file.exists(candidate_file)) {
        return(normalise_web_path(candidate_path))
      }

      fallback_path <- "assets/marker.svg"
      fallback_file <- resolve_asset_path(fallback_path)

      if (file.exists(fallback_file)) {
        return(normalise_web_path(fallback_path))
      }

      ""
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )

  icons(
    iconUrl = marker_urls,
    iconWidth = 24,
    iconHeight = 24,
    iconAnchorX = 12,
    iconAnchorY = 24,
    className = "bird-marker-icon"
  )
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
    max_radius <= 15 ~ list(
      radii = c(
        max_radius * 200,
        max_radius * 400,
        max_radius * 600,
        max_radius * 800,
        max_radius * 1000
      ),
      opacities = c(0.20, 0.15, 0.10, 0.07, 0.04)
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
    "https://api.mapbox.com/styles/v1/mapbox/light-v11",
    "/tiles/{z}/{x}/{y}",
    "?access_token=",
    "pk.eyJ1IjoiaGs3NDAyIiwiYSI6ImNtaGJkM3BxdTB3bGQyaXB5czY2ZW1zMG0ifQ",
    ".yD4Rsrn1vPqxXk2AFgjOZA"
  )

  # Get icon paths for each bird based on taxonomic order
  marker_orders <- map_data$order
  marker_rarities <- if ("rarityCategory" %in% names(map_data)) {
    map_data$rarityCategory
  } else {
    rep(NA_character_, nrow(map_data))
  }
  marker_icons <- compose_marker_icons(marker_orders, marker_rarities)

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
      layerId = ~marker_id,
      clusterOptions = leaflet::markerClusterOptions(
        disableClusteringAtZoom = 15,
        spiderfyOnMaxZoom = TRUE,
        removeOutsideVisibleBounds = TRUE,
        maxClusterRadius = 80
      ),
      clusterId = "bird_clusters"
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
