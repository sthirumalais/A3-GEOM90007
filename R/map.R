library(dplyr)
library(leaflet)
library(htmltools)

map_symbol <- function(order = "default") {
  if (order == "location") {
    return("assets/location.svg")
  }
  "assets/marker.svg"
}

compose_marker_icons <- function(orders, rarities) {
  if (length(orders) == 0) {
    return(icons(iconUrl = character(0)))
  }

  normalise_web_path <- function(path) {
    # Replace spaces to keep URLs safe while preserving directory separators
    gsub(" ", "%20", path, fixed = TRUE)
  }

  marker_urls <- vapply(
    seq_along(orders),
    FUN = function(idx) {
      order <- orders[[idx]]
      rarity <- rarities[[idx]]

      candidate_path <- sprintf(
        "bird-data/Markers/%s_%s.svg",
        order,
        rarity
      )
      normalise_web_path(candidate_path)
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

  if (max_radius == 0) {
    return(list(radii = 0, opacities = 0))
  }

  if (max_radius <= 2.5) {
    return(list(radii = c(max_radius * 1000), opacities = c(0.15)))
  }

  if (max_radius <= 5) {
    return(list(
      radii = c(max_radius * 500, max_radius * 1000),
      opacities = c(0.12, 0.08)
    ))
  }

  if (max_radius <= 7.5) {
    return(list(
      radii = c(max_radius * 333, max_radius * 666, max_radius * 1000),
      opacities = c(0.15, 0.10, 0.07)
    ))
  }

  if (max_radius <= 10) {
    return(list(
      radii = c(
        max_radius * 250,
        max_radius * 500,
        max_radius * 750,
        max_radius * 1000
      ),
      opacities = c(0.22, 0.13, 0.08, 0.05)
    ))
  }

  if (max_radius <= 15) {
    return(list(
      radii = c(
        max_radius * 200,
        max_radius * 400,
        max_radius * 600,
        max_radius * 800,
        max_radius * 1000
      ),
      opacities = c(0.20, 0.15, 0.10, 0.07, 0.04)
    ))
  }

  list(radii = 0, opacities = 0)
}

map_renderer <- function(map_data, state) {
  center_lat <- state$center_lat
  center_lng <- state$center_lng
  zoom_level <- state$zoom_level
  radius_range <- state$filter_radius

  radar_info <- get_radar_info(radius_range)

  # Map tiles by Mapbox
  # https://www.mapbox.com/
  # Map data © OpenStreetMap contributors
  # https://www.openstreetmap.org/copyright
  mapbox_template <- paste0(
    "https://api.mapbox.com/styles/v1/mapbox/light-v11",
    "/tiles/{z}/{x}/{y}",
    "?access_token=",
    "pk.eyJ1IjoiaGs3NDAyIiwiYSI6ImNtaGJkM3BxdTB3bGQyaXB5czY2ZW1zMG0ifQ",
    ".yD4Rsrn1vPqxXk2AFgjOZA"
  )

  marker_orders <- map_data$order
  marker_rarities <- map_data$rarityCategory
  marker_icons <- compose_marker_icons(marker_orders, marker_rarities)

  map <- leaflet::leaflet(
    data = map_data,
    options = leaflet::leafletOptions(
      minZoom = 10,
      maxZoom = 18
    ),
    sizingPolicy = leaflet::leafletSizingPolicy(
      defaultWidth = "100%",
      defaultHeight = "100%"
    )
  ) %>%
    leaflet::addTiles(
      urlTemplate = mapbox_template,
      attribution = "© Mapbox © OpenStreetMap",
      options = leaflet::tileOptions(
        minZoom = 10,
        maxZoom = 18
      )
    ) %>%
    setView(lng = center_lng, lat = center_lat, zoom = zoom_level) %>%
    # Marker clustering provided by Leaflet.markercluster plugin
    # https://github.com/Leaflet/Leaflet.markercluster
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
    )

  if (length(radar_info$radii) > 0 && radar_info$radii[1] > 0) {
    for (i in seq_along(radar_info$radii)) {
      map <- map %>%
        leaflet::addCircles(
          lat = center_lat,
          lng = center_lng,
          radius = radar_info$radii[i],
          color = "#7CB342",
          fillColor = "#7CB342",
          fillOpacity = radar_info$opacities[i],
          weight = 0.2,
          stroke = TRUE
        )
    }
  }

  map <- map %>%
    leaflet::addScaleBar(
      position = "bottomleft",
      options = scaleBarOptions(
        metric = TRUE,
        imperial = FALSE
      )
    ) %>%
    leaflet::addControl(
      html = htmltools::tags$img(
        width = 36,
        height = 36,
        src = "assets/north.svg"
      ),
      position = "bottomright",
      className = "leaflet-control-north-arrow"
    )

  map
}
