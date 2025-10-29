################################################################################
# Server components for the dashboard                                          #
################################################################################
library(dplyr)
library(shiny)
library(leaflet)
library(jsonlite)

# Helper utilities ------------------------------------------------------------

#' Resolve a media path to an actual file (handles directory inputs)
#' @param path character path from the data set
#' @param pattern optional regex pattern for files of interest
#' @return character path to the resolved file or NULL if not found
resolve_media_path <- function(path, pattern = NULL) {
  if (is.null(path) || length(path) == 0) {
    return(NULL)
  }

  path <- as.character(path[1])
  if (is.na(path)) {
    return(NULL)
  }

  if (!nzchar(path) || tolower(path) %in% c("no audio data", "no image data")) {
    return(NULL)
  }

  # Path might be a directory in the CSV so try to pick the first matching file
  if (dir.exists(path)) {
    files <- list.files(path, pattern = pattern, ignore.case = TRUE, full.names = TRUE)
    if (length(files) == 0) {
      return(NULL)
    }
    return(files[1])
  }

  if (file.exists(path)) {
    return(path)
  }

  return(NULL)
}

#' Convert a filesystem media path to the exposed Shiny resource path
#' @param path filesystem path under Data/
#' @return web-accessible path using the bird-data resource prefix
to_web_media_path <- function(path) {
  sub("^Data/", "bird-data/", path)
}


#' The server function to pass to the shiny dashboard
server <- function(input, output, session) {
  #' State -------------------------------------------------------------------

  #' Reactive states and settable defaults
  state <- reactiveValues()

  #' @param ui_colors {list} List of colors used in the app.
  state$ui_colors <- list(
    "background" = "#FFFFFF",
    "lightgray" = "#E9E9EA",
    "gray" = "#A8A8B5",
    "darkgray" = "#949598",
    "foreground" = "#52525F",
    "accent" = "#2D8B57",      # Forest green for bird/nature theme
    "highlight" = "#7CB342"    # Light green for highlights
  )

  #' @param fonts {list} List of fonts used in the app.
  state$fonts <- list(
    "primary" = "'brandon-grotesque', 'Helvetica', 'Arial', sans-serif"
  )

  #' @param center_lat {numeric} The center latitude for the map
  state$center_lat <- -37.8136

  #' @param center_lng {numeric} The center longitude for the map
  state$center_lng <- 144.9631

  #' @param zoom_level {numeric} The initial zoom level
  state$zoom_level <- 12

  #' @param filter_species {character} Filter for selected bird species
  state$filter_species <- character(0)

  #' @param filter_order {character} Filter for selected bird taxonomic orders
  state$filter_order <- character(0)

  #' @param filter_rarity {vector} Rarity categories to show
  state$filter_rarity <- c("Common", "Fairly Common", "Uncommon", "Rare", "Vagrant")

  #' @param filter_year_range {c(min, max)} Filter for observation years
  state$filter_year_range <- c(1985, 2019)

  #' @param filter_radius {c(min, max)} Distance range from center location in km
  state$filter_radius <- c(0, 8)

  #' Event handlers ----------------------------------------------------------

  # Handle incoming messages from Javascript (GPS location)
  observeEvent(input$js_set_loc, {
    print("Set incoming location from JS client")
    lat <- as.numeric(input$js_set_loc$lat)
    lon <- as.numeric(input$js_set_loc$lon)
    state$center_lat <- lat
    state$center_lng <- lon
    state$zoom_level <- 14
  })

  # Filter by species
  observeEvent(input$filter_species, {
    state$filter_species <- if (is.null(input$filter_species)) character(0) else input$filter_species
  }, ignoreNULL = FALSE)

  # Filter by order
  observeEvent(input$filter_order, {
    state$filter_order <- if (is.null(input$filter_order)) character(0) else input$filter_order
  }, ignoreNULL = FALSE)

  # Filter by rarity checkboxes
  observe({
    rarity_categories <- c()
    if (isTRUE(input$filter_common)) rarity_categories <- c(rarity_categories, "Common")
    if (isTRUE(input$filter_fairly_common)) rarity_categories <- c(rarity_categories, "Fairly Common")
    if (isTRUE(input$filter_uncommon)) rarity_categories <- c(rarity_categories, "Uncommon")
    if (isTRUE(input$filter_rare)) rarity_categories <- c(rarity_categories, "Rare")
    if (isTRUE(input$filter_vagrant)) rarity_categories <- c(rarity_categories, "Vagrant")
    state$filter_rarity <- rarity_categories
  })

  # Filter by year range
  observeEvent(input$filter_year_range, {
    state$filter_year_range <- input$filter_year_range
  })

  # Filter by radius
  observeEvent(input$filter_radius, {
    state$filter_radius <- input$filter_radius
  })

  #' Master data --------------------------------------------------------------

  # Load the bird sighting data
  bird_data <- load_bird_data()
  full_year_range <- c(1985, 2019)
  state$filter_year_range <- full_year_range

  taxonomy_lookup <- bird_data %>%
    distinct(commonName, order) %>%
    arrange(order, commonName)

  available_species <- reactive({
    selected_orders <- state$filter_order
    lookup <- taxonomy_lookup

    if (length(selected_orders) > 0) {
      lookup <- lookup %>% filter(order %in% selected_orders)
    }

    lookup %>%
      distinct(commonName) %>%
      arrange(commonName) %>%
      pull(commonName)
  })

  available_orders <- reactive({
    selected_species <- state$filter_species
    lookup <- taxonomy_lookup

    if (length(selected_species) > 0) {
      lookup <- lookup %>% filter(commonName %in% selected_species)
    }

    lookup %>%
      distinct(order) %>%
      arrange(order) %>%
      pull(order)
  })

  observeEvent(available_species(), {
    choices <- available_species()
    current_selection <- isolate(state$filter_species)
    valid_selection <- intersect(current_selection, choices)

    if (!identical(current_selection, valid_selection)) {
      state$filter_species <- valid_selection
    }

    updateSelectInput(
      session,
      "filter_species",
      choices = choices,
      selected = if (length(valid_selection) == 0) NULL else valid_selection
    )
  }, ignoreNULL = FALSE)

  observeEvent(available_orders(), {
    choices <- available_orders()
    current_selection <- isolate(state$filter_order)
    valid_selection <- intersect(current_selection, choices)

    # Only update state$filter_order if species are selected
    # This prevents clearing order filters when user selects orders without species
    if (length(isolate(state$filter_species)) > 0) {
      if (!identical(current_selection, valid_selection)) {
        state$filter_order <- valid_selection
      }
    }

    updateSelectInput(
      session,
      "filter_order",
      choices = choices,
      selected = if (length(valid_selection) == 0) NULL else valid_selection
    )
  }, ignoreNULL = FALSE)

  # Update filter controls with actual data
  observe({
    updateSliderInput(
      session,
      "filter_year_range",
      min = full_year_range[1],
      max = full_year_range[2],
      value = state$filter_year_range
    )
  })

  #' Filtered dataset reacting to user controls
  filtered_data <- reactive({
    species_filter <- state$filter_species

    # When only taxonomic orders are selected, automatically constrain the map
    # to the species that belong to those orders.
    if (length(species_filter) == 0 && length(state$filter_order) > 0) {
      species_filter <- available_species()
    }

    filter_bird_data(
      bird_data,
      species = species_filter,
      order = state$filter_order,
      year_range = state$filter_year_range,
      radius_range = state$filter_radius,
      center_location = c(state$center_lat, state$center_lng),
      rarity_filters = state$filter_rarity
    )
  })

  #' Mapping -----------------------------------------------------------------

  #' Render the world map in a leaflet widget
  output$leaflet_map <- renderLeaflet({
    map_renderer(filtered_data(), state)
  })

  #' Observe map zoom and update map markers dynamically
  observe({
    data <- filtered_data()

    if (nrow(data) == 0) {
      # Create location marker icon
      location_icon <- icons(
        iconUrl = map_symbol("location"),
        iconWidth = 24,
        iconHeight = 24
      )

      leafletProxy("leaflet_map") %>%
        clearMarkers() %>%
        clearMarkerClusters() %>%
        leaflet::addMarkers(
          lat = state$center_lat,
          lng = state$center_lng,
          icon = location_icon,
          options = markerOptions(clickable = FALSE)
        ) %>%
        leaflet::setView(
          lng = state$center_lng,
          lat = state$center_lat,
          zoom = state$zoom_level
        )
      return()
    }

    # Build composite icons that overlay order imagery on rarity markers
    marker_icons <- compose_marker_icons(
      orders = data$order,
      rarities = if ("rarityCategory" %in% names(data)) {
        data$rarityCategory
      } else {
        rep(NA_character_, nrow(data))
      }
    )

    leafletProxy("leaflet_map", data = data) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      # Bird Sighting Markers
      leaflet::addMarkers(
        ~ longitude, ~ latitude,
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
      leaflet::setView(
        lng = state$center_lng,
        lat = state$center_lat,
        zoom = state$zoom_level
      )
  })

  #' Handle marker clicks to show detailed information
  observeEvent(input$leaflet_map_marker_click, {
    clicked <- input$leaflet_map_marker_click
    req(clicked$id)

    data <- filtered_data()

    # Use marker_id to find the exact bird that was clicked
    # This works correctly even when markers are spiderfied (in spiral formation)
    selected_bird <- data %>%
      filter(marker_id == as.numeric(clicked$id))

    if (nrow(selected_bird) > 0) {
      # Resolve media paths
      image_file <- resolve_media_path(
        selected_bird$imagePath,
        pattern = "\\.(png|jpg|jpeg|gif)$"
      )
      audio_file <- resolve_media_path(
        selected_bird$audioPath,
        pattern = "\\.(mp3|wav|ogg|m4a)$"
      )
      credits_file <- resolve_media_path(
        selected_bird$creditsPath,
        pattern = "\\.(json)$"
      )

      modal_children <- list(class = "bird-detail-modal")

      if (!is.null(image_file)) {
        modal_children <- c(
          modal_children,
          list(
            tags$div(
              class = "bird-image",
              tags$img(
                src = to_web_media_path(image_file),
                alt = selected_bird$commonName,
                style = "max-width: 100%; border-radius: 10px; margin-bottom: 15px; display: block; margin-left: auto; margin-right: auto;"
              )
            )
          )
        )
      }

      # Create mermaid diagram for taxonomy
      mermaid_id <- paste0("mermaid-", gsub("[^A-Za-z0-9]", "", selected_bird$commonName))
      mermaid_diagram <- sprintf("
        graph TD
          A[Order<br/><b>%s</b>]
          B[Family<br/><b>%s</b>]
          C[Genus<br/><b>%s</b>]
          A --> B
          B --> C

          classDef orderStyle fill:#E8F5E9,stroke:#4CAF50,stroke-width:3px,color:#2D5016
          classDef familyStyle fill:#C8E6C9,stroke:#66BB6A,stroke-width:2px,color:#2D5016
          classDef genusStyle fill:#A5D6A7,stroke:#81C784,stroke-width:2px,color:#2D5016

          class A orderStyle
          class B familyStyle
          class C genusStyle
      ",
      selected_bird$order,
      selected_bird$family,
      selected_bird$genus)

      mermaid_container <- tags$div(
        class = "mermaid",
        id = mermaid_id,
        style = "text-align: center; background: #fafafa; padding: 15px; border-radius: 8px;"
      )
      mermaid_container <- htmltools::tagAppendAttributes(
        mermaid_container,
        `data-mermaid-definition` = mermaid_diagram
      )

      modal_children <- c(
        modal_children,
        list(
          tags$p(
            style = "text-align: center; margin: 15px 0; font-size: 16px;",
            tags$em(selected_bird$scientificName)
          ),
          tags$hr(),
          tags$div(
            class = "taxonomy-section",
            style = "margin: 20px 0;",
            tags$h4(
              style = "text-align: center; margin-bottom: 15px; font-size: 16px;",
              "Taxonomic Classification"
            ),
            mermaid_container
          ),
          tags$hr(),
          tags$p(
            tags$strong("Rarity: "),
            tags$span(
              style = paste0("color: ", case_when(
                selected_bird$rarityCategory == "Common" ~ "#4CAF50",
                selected_bird$rarityCategory == "Fairly Common" ~ "#FFC107",
                selected_bird$rarityCategory == "Uncommon" ~ "#FF9800",
                selected_bird$rarityCategory == "Rare" ~ "#F44336",
                selected_bird$rarityCategory == "Vagrant" ~ "#9C27B0",
                TRUE ~ "#52525F"
              ), "; font-weight: 600;"),
              selected_bird$rarityCategory
            )
          )
        )
      )

      if (!is.na(selected_bird$description) && nzchar(selected_bird$description)) {
        modal_children <- c(
          modal_children,
          list(
            tags$div(
              tags$hr(),
              tags$p(selected_bird$description)
            )
          )
        )
      }

      if (!is.null(audio_file)) {
        audio_children <- list(
          class = "bird-audio",
          tags$hr(),
          tags$h4("Bird Call"),
          tags$audio(
            controls = "controls",
            style = "width: 100%; margin: 10px 0;",
            tags$source(
              src = to_web_media_path(audio_file),
              type = "audio/mpeg"
            )
          )
        )

        if (!is.null(credits_file)) {
          credits <- jsonlite::fromJSON(credits_file)
          info <- credits$citation_info
          audio_children <- c(
            audio_children,
            list(
              tags$p(
                class = "audio-credits",
                style = "font-size: 12px; color: #666;",
                tags$a(
                  href = info$recording_url,
                  target = "_blank",
                  "Recording"
                ),
                " by ", info$recorder, " | ",
                tags$a(
                  href = info$license_url,
                  target = "_blank",
                  info$license_type
                )
              )
            )
          )
        }

        modal_children <- c(
          modal_children,
          list(do.call(tags$div, audio_children))
        )
      }

      modal_content <- do.call(tags$div, modal_children)

      showModal(modalDialog(
        title = tags$div(style = "text-align: center;", selected_bird$commonName),
        modal_content,
        easyClose = TRUE,
        footer = modalButton("Close")
      ))

      session$sendCustomMessage(
        type = "renderMermaid",
        message = list(
          id = mermaid_id,
          definition = mermaid_diagram
        )
      )
    }
  })
}
