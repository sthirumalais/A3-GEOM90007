################################################################################
# Libraries required for the app                                               #
################################################################################

#' Dependencies used in the app
dependencies <- c(
  "shiny",
  "dplyr",
  "readr",
  "leaflet",
  "htmltools",
  "glue",
  "lubridate", # For date handling and year extraction
  "jsonlite" # For parsing audio credits JSON
)

#' Attempts to load packages and install them if required
#' @param dependencies - a vector of dependency names
load_dependencies <- function(dependencies) {
  print("Loading dependencies...")
  for (package in dependencies) {
    tryCatch(
      {
        library(package, character.only = TRUE)
        print(sprintf("Loaded %s", package))
      },
      error = function(e) {
        message(sprintf("Installing %s...", package))
        install.packages(package, repos = "https://cloud.r-project.org")
        library(package, character.only = TRUE)
        print(sprintf("Installed and loaded %s", package))
      }
    )
  }
  print("All dependencies loaded successfully!")
}

# Load dependencies
load_dependencies(dependencies)
