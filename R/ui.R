################################################################################
# UI components for the dashboard                                              #
################################################################################
library(shiny)
library(leaflet)
library(glue)
library(htmltools)

# Headers----------------------------------------------------------------------

headers <- tags$head(
  # favicon
  tags$link(
    rel = "icon",
    type = "image/x-icon",
    href = "assets/favicon.svg"
  ),
  # web fonts
  tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "https://use.typekit.net/zvh8ynu.css"
  ),
  tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "https://fonts.googleapis.com/icon?family=Material+Icons"
  ),
  # css overrides
  tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "assets/shiny_app.css"
  ),
  # javascript
  tags$script(
    src = "assets/shiny_app.js"
  )
)

# Filter Panel-----------------------------------------------------------------

filter_panel <- tabPanel(
  title = "Filters",
  fluidRow(
    class = "header",
    tags$h1(
      "Filters"
    ),
    tags$div(
      id = "filters-show-hide",
      class = "button-show",
    )
  ),
  tags$div(class = "spacer h32"),
  fluidRow(
    class = "control",
    selectInput(
      inputId = "filter_species",
      label = "Species",
      choices = character(0),
      selected = NULL,
      multiple = TRUE
    )
  ),
  tags$div(class = "spacer h32"),
  fluidRow(
    class = "control",
    selectInput(
      inputId = "filter_order",
      label = "Order (Taxonomy)",
      choices = character(0),
      selected = NULL,
      multiple = TRUE
    )
  ),
  tags$div(class = "spacer h32"),
  fluidRow(
    class = "control",
    checkboxInput(
      inputId = "filter_common",
      label = "Show Common birds",
      value = TRUE
    )
  ),
  fluidRow(
    class = "control",
    checkboxInput(
      inputId = "filter_fairly_common",
      label = "Show Fairly Common birds",
      value = TRUE
    )
  ),
  fluidRow(
    class = "control",
    checkboxInput(
      inputId = "filter_uncommon",
      label = "Show Uncommon birds",
      value = TRUE
    )
  ),
  fluidRow(
    class = "control",
    checkboxInput(
      inputId = "filter_rare",
      label = "Show Rare birds",
      value = TRUE
    )
  ),
  fluidRow(
    class = "control",
    checkboxInput(
      inputId = "filter_vagrant",
      label = "Show Vagrant birds",
      value = TRUE
    )
  ),
  tags$div(class = "spacer h32"),
  fluidRow(
    class = "control",
    tags$div(
      class = "label",
      "Observation Period"
    ),
    sliderInput(
      inputId = "filter_year_range",
      min = 1985,
      max = 2019,
      value = c(1985, 2019),
      step = 1,
      sep = "",
      label = NULL
    )
  ),
  tags$div(class = "spacer h32"),
  fluidRow(
    class = "control",
    tags$div(
      class = "label",
      "Search Radius"
    ),
    sliderInput(
      inputId = "filter_radius",
      min = 0,
      max = 8,
      step = 2,
      value = c(0, 8),
      dragRange = TRUE,
      label = NULL,
      post = "km"
    )
  ),
  tags$div(class = "spacer h32")
)

# Dimmer Panel-----------------------------------------------------------------

dimmer_panel <- tabPanel(
  title = "Dimmer"
)

# Map Panel---------------------------------------------------------------------

map_panel <- tabPanel(
  title = "Map",
  leafletOutput(
    "leaflet_map",
    height = "100%",
    width = "100%"
  )
)

# Search Panel-----------------------------------------------------------------

search_panel <- tabPanel(
  title = "Search",
  fluidRow(
    class = "logo",
    tags$img(
      src = "assets/logo.svg"
    )
  ),
  fluidRow(
    class = "search-bar",
    tags$div(
      class = "wrapper",
      textInput(
        inputId = "search-input",
        label = NULL,
        value = "Melbourne",
        placeholder = "Search Destination"
      ),
      tags$div(
        id = "button-gps",
        class = "button gps"
      ),
      tags$div(
        id = "button-search",
        class = "button search"
      )
    )
  )
)

search_results_panel <- tabPanel(
  title = "SearchResults"
)

# Intro panel------------------------------------------------------------------

intro_panel <- tabPanel(
  title = "Intro",
  class = "page-1",
  tags$img(class = "logo", src = "assets/naarmwings-logo.svg"),
  tags$div(
    class = "pages",
    tags$div(
      class = "page",
      tags$img(src = "assets/slide1.svg"),
      tags$p(
        "Explore bird observations across Melbourne (Naarm) from 1985 to 2019."
      )
    ),
    tags$div(
      class = "page",
      tags$img(src = "assets/slide2.svg"),
      tags$p(
        "Find species near your location, filter by time period, rarity, and more."
      )
    ),
    tags$div(
      class = "page",
      tags$img(src = "assets/slide3.svg"),
      tags$p(
        "Discover the rich avian biodiversity of Melbourne with interactive maps and sounds."
      )
    )
  ),
  tags$div(class = "bubble one"),
  tags$div(class = "bubble two"),
  tags$div(
    class = "dots",
    tags$div(class = "dot seq-1"),
    tags$div(class = "dot seq-2"),
    tags$div(class = "dot seq-3")
  ),
  tags$div(
    id = "intro-left",
    class = "button left",
    direction = "left"
  ),
  tags$div(
    id = "intro-right",
    class = "button right",
    direction = "right"
  )
)

# Loading panel----------------------------------------------------------------

loading_panel <- tabPanel(
  title = "Loading",
  class = "container",
  tags$div(
    class = "pulse"
  )
)

# UI element-------------------------------------------------------------------

ui <- tagList(
  headers,
  navbarPage(
    title = "NaarmWings",
    map_panel,
    search_panel,
    search_results_panel,
    dimmer_panel,
    loading_panel,
    filter_panel,
    intro_panel,
    header = NULL,
    windowTitle = "NaarmWings",
    fluid = FALSE,
    position = "fixed-top",
    lang = "en"
  )
)
