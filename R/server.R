library(dplyr)
library(shiny)
library(leaflet)
library(jsonlite)
library(rlang)

modal_image_style <- paste(
  "max-width: 100%; border-radius: 10px; margin-bottom: 15px;",
  "display: block; margin-left: auto; margin-right: auto;"
)

mermaid_container_style <- paste(
  "text-align: center; background: #fafafa; padding: 15px;",
  "border-radius: 8px;"
)

mermaid_heading_style <- paste(
  "text-align: center; margin-bottom: 15px; font-size: 16px;"
)

modal_title_style <- paste(
  "text-align: center; margin: 15px 0; font-size: 16px;"
)

tableau_container_small <- "width: 100%; height: 300px; overflow: hidden;"
tableau_container_large <- "width: 100%; height: 400px; overflow: hidden;"
tableau_frame_small <- "width: 100%; height: 300px; display: block;"
tableau_frame_large <- "width: 100%; height: 400px; display: block;"

utils::globalVariables(c(
  "map_renderer",
  "map_symbol",
  "compose_marker_icons",
  "tableauPublicViz",
  "marker_id",
  "load_bird_data",
  "filter_bird_data"
))

run_delayed_js <- function(delay_ms, call_expr) {
  shinyjs::runjs(
    sprintf("setTimeout(() => %s, %d);", call_expr, delay_ms)
  )
}


server <- function(input, output, session) {
  state <- reactiveValues()

  state$ui_colors <- list(
    "background" = "#FFFFFF",
    "lightgray" = "#E9E9EA",
    "gray" = "#A8A8B5",
    "darkgray" = "#949598",
    "foreground" = "#52525F",
    "accent" = "#2D8B57",
    "highlight" = "#7CB342"
  )

  state$fonts <- list(
    "primary" = "'brandon-grotesque', 'Helvetica', 'Arial', sans-serif"
  )

  state$center_lat <- -37.8136

  state$center_lng <- 144.9631

  state$zoom_level <- 12

  state$filter_species <- character(0)

  state$filter_order <- character(0)

  state$filter_rarity <- c(
    "Common",
    "Fairly Common",
    "Uncommon",
    "Rare",
    "Vagrant"
  )

  state$filter_year_range <- c(1998, 2019)

  state$filter_radius <- c(0, 8)

  observeEvent(input$js_set_loc, {
    lat <- as.numeric(input$js_set_loc$lat)
    lon <- as.numeric(input$js_set_loc$lon)
    state$center_lat <- lat
    state$center_lng <- lon
    state$zoom_level <- 14
  })

  observeEvent(
    input$filter_species,
    {
      state$filter_species <- if (is.null(input$filter_species)) {
        character(0)
      } else {
        input$filter_species
      }
    },
    ignoreNULL = FALSE
  )

  observeEvent(
    input$filter_order,
    {
      state$filter_order <- if (is.null(input$filter_order)) {
        character(0)
      } else {
        input$filter_order
      }
    },
    ignoreNULL = FALSE
  )

  observe({
    rarity_categories <- c()
    if (isTRUE(input$filter_common)) {
      rarity_categories <- c(rarity_categories, "Common")
    }
    if (isTRUE(input$filter_fairly_common)) {
      rarity_categories <- c(rarity_categories, "Fairly Common")
    }
    if (isTRUE(input$filter_uncommon)) {
      rarity_categories <- c(rarity_categories, "Uncommon")
    }
    if (isTRUE(input$filter_rare)) {
      rarity_categories <- c(rarity_categories, "Rare")
    }
    if (isTRUE(input$filter_vagrant)) {
      rarity_categories <- c(rarity_categories, "Vagrant")
    }
    state$filter_rarity <- rarity_categories
  })

  observeEvent(input$filter_year_range, {
    state$filter_year_range <- input$filter_year_range
    # keep Tableau viz (if present) synced with year range
    shinyjs::runjs(
      sprintf(
        'window.updateTableauYearRange("%s", %d, %d);',
        "tableauSightingsByYear",
        as.integer(input$filter_year_range[1]),
        as.integer(input$filter_year_range[2])
      )
    )
  })

  observeEvent(input$filter_radius, {
    state$filter_radius <- c(0, input$filter_radius)
  })

  bird_data <- load_bird_data() # nolint: object_usage_linter
  full_year_range <- c(1998, 2019)
  state$filter_year_range <- full_year_range

  taxonomy_lookup <- bird_data %>%
    distinct(.data$commonName, .data$order) %>%
    arrange(.data$order, .data$commonName)

  available_species <- reactive({
    selected_orders <- state$filter_order
    lookup <- taxonomy_lookup

    if (length(selected_orders) > 0) {
      lookup <- lookup %>% filter(.data$order %in% selected_orders)
    }

    lookup %>%
      distinct(.data$commonName) %>%
      arrange(.data$commonName) %>%
      pull(.data$commonName)
  })

  available_orders <- reactive({
    selected_species <- state$filter_species
    lookup <- taxonomy_lookup

    if (length(selected_species) > 0) {
      lookup <- lookup %>% filter(.data$commonName %in% selected_species)
    }

    lookup %>%
      distinct(.data$order) %>%
      arrange(.data$order) %>%
      pull(.data$order)
  })

  observeEvent(
    available_species(),
    {
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
    },
    ignoreNULL = FALSE
  )

  observeEvent(
    available_orders(),
    {
      choices <- available_orders()
      current_selection <- isolate(state$filter_order)
      valid_selection <- intersect(current_selection, choices)

      # Only update order filter when species are selected to retain choices
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
    },
    ignoreNULL = FALSE
  )

  observe({
    updateSliderInput(
      session,
      "filter_year_range",
      min = full_year_range[1],
      max = full_year_range[2],
      value = state$filter_year_range
    )
  })

  # Filtered dataset reacting to user controls
  filtered_data <- reactive({
    species_filter <- state$filter_species

    # When only taxonomic orders are selected, automatically constrain the map
    # to the species that belong to those orders.
    if (length(species_filter) == 0 && length(state$filter_order) > 0) {
      species_filter <- available_species()
    }

    filter_bird_data( # nolint: object_usage_linter
      bird_data,
      species = species_filter,
      order = state$filter_order,
      year_range = state$filter_year_range,
      radius_range = state$filter_radius,
      center_location = c(state$center_lat, state$center_lng),
      rarity_filters = state$filter_rarity
    )
  })

  output$leaflet_map <- renderLeaflet({
    map_renderer(filtered_data(), state) # nolint: object_usage_linter
  })

  observe({
    data <- filtered_data()

    if (nrow(data) == 0) {
      location_icon <- icons(
        iconUrl = map_symbol("location"), # nolint: object_usage_linter
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
    marker_icons <- compose_marker_icons( # nolint: object_usage_linter
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
      leaflet::setView(
        lng = state$center_lng,
        lat = state$center_lat,
        zoom = state$zoom_level
      )
  })

  observeEvent(input$leaflet_map_marker_click, {
    clicked <- input$leaflet_map_marker_click
    req(clicked$id)

    data <- filtered_data()

    # Use marker_id to find the exact bird that was clicked
    # Works even when markers are spiderfied in clusters
    selected_bird <- data %>%
      filter(.data$marker_id == as.numeric(clicked$id))

    req(nrow(selected_bird) > 0)

    image_src <- selected_bird$image_src[[1]]
    audio_file <- selected_bird$audio_file[[1]]
    audio_src <- selected_bird$audio_src[[1]]
    credits_file <- selected_bird$credits_file[[1]]

    modal_children <- list(class = "bird-detail-modal")

    if (!is.na(image_src)) {
      modal_children <- c(
        modal_children,
        list(
          tags$div(
            class = "bird-image",
            tags$img(
              src = image_src,
              alt = selected_bird$commonName,
              style = modal_image_style
            )
          )
        )
      )
    }

    # Create mermaid diagram for taxonomy
    mermaid_slug <- gsub("[^A-Za-z0-9]", "", selected_bird$commonName)
    mermaid_id <- paste0("mermaid-", mermaid_slug)
    mermaid_template <- paste(
      "graph TD",
      "  A[Order<br/><b>%s</b>]",
      "  B[Family<br/><b>%s</b>]",
      "  C[Genus<br/><b>%s</b>]",
      "  A --> B",
      "  B --> C",
      "",
      paste(
        "  classDef orderStyle",
        "fill:#E8F5E9,stroke:#4CAF50,stroke-width:3px,color:#2D5016"
      ),
      paste(
        "  classDef familyStyle",
        "fill:#C8E6C9,stroke:#66BB6A,stroke-width:2px,color:#2D5016"
      ),
      paste(
        "  classDef genusStyle",
        "fill:#A5D6A7,stroke:#81C784,stroke-width:2px,color:#2D5016"
      ),
      "  class A orderStyle",
      "  class B familyStyle",
      "  class C genusStyle",
      sep = "\n"
    )
    mermaid_diagram <- sprintf(
      mermaid_template,
      selected_bird$order,
      selected_bird$family,
      selected_bird$genus
    )

    mermaid_container <- tags$div(
      class = "mermaid",
      id = mermaid_id,
      style = mermaid_container_style
    )
    mermaid_container <- htmltools::tagAppendAttributes(
      mermaid_container,
      `data-mermaid-definition` = mermaid_diagram
    )

    modal_children <- c(
      modal_children,
      list(
        tags$p(
          style = modal_title_style,
          tags$em(selected_bird$scientificName)
        ),
        tags$hr(),
        tags$div(
          class = "taxonomy-section",
          style = "margin: 20px 0;",
          tags$h4(
            style = mermaid_heading_style,
            "Taxonomic Classification"
          ),
          mermaid_container
        ),
        tags$hr(),
        tags$p(
          tags$strong("Rarity: "),
          {
            rarity_colour <- case_when(
              selected_bird$rarityCategory == "Common" ~ "#4CAF50",
              selected_bird$rarityCategory == "Fairly Common" ~ "#FFC107",
              selected_bird$rarityCategory == "Uncommon" ~ "#FF9800",
              selected_bird$rarityCategory == "Rare" ~ "#F44336",
              selected_bird$rarityCategory == "Vagrant" ~ "#9C27B0"
            )
            tags$span(
              style = paste0("color: ", rarity_colour, "; font-weight: 600;"),
              selected_bird$rarityCategory
            )
          }
        )
      )
    )

    modal_children <- c(
      modal_children,
      list(
        tags$div(
          tags$hr(),
          tags$p(selected_bird$description)
        )
      )
    )

    if (!is.na(audio_file)) {
      audio_children <- list(
        class = "bird-audio",
        tags$hr(),
        tags$h4("Bird Call"),
        tags$audio(
          controls = "controls",
          style = "width: 100%; margin: 10px 0;",
          tags$source(
            src = audio_src,
            type = "audio/mpeg"
          )
        )
      )

      if (!is.na(credits_file)) {
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
              " by ",
              info$recorder,
              " | ",
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

    # Tableau Sightings by Year (interactive)
    tableau_id <- "tableauSightingsByYear"
    tableau_url <- paste0(
      "https://public.tableau.com/views/BirdSightings/Sightingsbyyear",
      "?:language=en-US&publish=yes&:sid=&:redirect=auth&:display_count=n&",
      ":origin=viz_share_link"
    )

    modal_children <- c(
      modal_children,
      list(
        tags$hr(),
        tags$h4("Sightings by Year"),
        tags$div(
          style = tableau_container_small,
          tableauPublicViz( # nolint: object_usage_linter
            id = tableau_id,
            url = tableau_url,
            height = "300px",
            style = tableau_frame_small,
            toolbar = "hidden"
          )
        )
      )
    )

    # Tableau Choropleth Map (interactive)
    # Use a constant ID so Shiny input bindings like
    # input$tableauChoropleth_mark_selection_changed remain consistent
    tableau_choropleth_id <- "tableauChoropleth"
    tableau_choropleth_url <- paste0(
      "https://public.tableau.com/views/Choropleth_birds/choropleth_birds",
      "?:language=en-US&publish=yes&:sid=&:redirect=auth&:display_count=n&",
      ":origin=viz_share_link"
    )

    modal_children <- c(
      modal_children,
      list(
        tags$hr(),
        tags$h4("Bird Sightings by Suburb"),
        tags$div(
          style = tableau_container_large,
          tableauPublicViz( # nolint: object_usage_linter
            id = tableau_choropleth_id,
            url = tableau_choropleth_url,
            height = "400px",
            style = tableau_frame_large,
            toolbar = "hidden"
          )
        )
      )
    )

    modal_content <- do.call(tags$div, modal_children)

    showModal(
      modalDialog(
        title = tags$div(
          style = "text-align: center;",
          selected_bird$commonName
        ),
        modal_content,
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )

    session$sendCustomMessage(
      type = "renderMermaid",
      message = list(
        id = mermaid_id,
        definition = mermaid_diagram
      )
    )

    # Sync tableau viz to current year range after modal opens
    run_delayed_js(
      300,
      sprintf(
        "window.updateTableauYearRange(\"%s\", %d, %d)",
        tableau_id,
        as.integer(state$filter_year_range[1]),
        as.integer(state$filter_year_range[2])
      )
    )

    # Apply scientific name filter to the Sightings by Year chart
    run_delayed_js(
      400,
      sprintf(
        "window.updateTableauScientificName(\"%s\", \"%s\")",
        tableau_id,
        selected_bird$scientificName
      )
    )

    # Set up choropleth listeners after modal opens
    event_observer_script <- paste(
      c(
        "{",
        "  if (window.observeTableauEvents) {",
        sprintf(
          "    window.observeTableauEvents(\"%s\");",
          tableau_choropleth_id
        ),
        "  }",
        "}"
      ),
      collapse = "\n"
    )
    run_delayed_js(500, event_observer_script)

    # Apply scientific name filter to the choropleth
    run_delayed_js(
      700,
      sprintf(
        "window.updateTableauScientificName(\"%s\", \"%s\")",
        tableau_choropleth_id,
        selected_bird$scientificName
      )
    )
  })

  # Handle suburb selection from choropleth map
  observeEvent(input$tableauChoropleth_mark_selection_changed, {
    selected_data <- input$tableauChoropleth_mark_selection_changed
    req(!is.null(selected_data))

    selected_df <- as.data.frame(selected_data, stringsAsFactors = FALSE)
    req(nrow(selected_df) > 0)

    suburb_lat <- suppressWarnings(
      # nolint: object_usage_linter
      as.numeric(selected_df[["ATTR(Centroid Latitude)"]][1])
    )
    suburb_lon <- suppressWarnings(
      as.numeric(selected_df[["ATTR(Centroid Longitude)"]][1])
    )
    suburb_name <- selected_df[["Loc Name"]][1]

    req(!is.na(suburb_lat), !is.na(suburb_lon))

    suburb_name <- ifelse(is.na(suburb_name), "", as.character(suburb_name))

    state$center_lat <- suburb_lat
    state$center_lng <- suburb_lon
    state$zoom_level <- 14

    if (nzchar(suburb_name)) {
      updateTextInput(session, "search-input", value = suburb_name)

      coords_js <- jsonlite::toJSON(
        list(lat = suburb_lat, lon = suburb_lon),
        auto_unbox = TRUE
      )
      suburb_js <- jsonlite::toJSON(suburb_name, auto_unbox = TRUE)

      search_update_script <- paste(
        c(
          "(function() {",
          "  const input = document.getElementById('search-input');",
          "  if (!input) { return; }",
          paste0("  const coords = ", coords_js, ";"),
          paste0("  input.value = ", suburb_js, ";"),
          "  input.dispatchEvent(",
          "    new CustomEvent('set:loc', { detail: coords })",
          "  );",
          "})();"
        ),
        collapse = "\n"
      )
      shinyjs::runjs(search_update_script)
    }

    removeModal()
  })
}
