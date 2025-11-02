# Main App File
# Sources all modules and runs the Shiny app

# Load dependencies first
source("R/libraries.R")

# Source all module files
source("R/tableau-in-shiny-v1.2.R")
source("R/data.R")
source("R/map.R")
source("R/ui.R")
source("R/server.R")

# Expose static assets (CSS/JS/images)
shiny::addResourcePath("assets", "www")

# Expose Data directory for images/audio in modals
shiny::addResourcePath("bird-data", "Data")

# Run the app
# Create app object
app <- shinyApp(ui = ui, server = server)

# Launch in external browser
runApp(app, launch.browser = TRUE)
