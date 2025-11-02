dependencies <- c(
  "shiny",
  "shinyjs",
  "dplyr",
  "readr",
  "leaflet",
  "htmltools",
  "lubridate", # For date handling and year extraction
  "jsonlite", # For parsing audio credits JSON
  "rlang"
)

load_dependencies <- function(dependencies) {
  for (package in dependencies) {
    tryCatch(
      {
        library(package, character.only = TRUE)
      },
      error = function(e) {
        message(sprintf("Installing %s...", package))
        install.packages(package, repos = "https://cloud.r-project.org")
        library(package, character.only = TRUE)
      }
    )
  }
}

load_dependencies(dependencies)
