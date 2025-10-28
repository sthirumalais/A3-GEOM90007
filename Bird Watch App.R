# Main App File
# Sources all modules and runs the Shiny app

library(shiny)

# Source all module files
source("R/data.R")
source("R/ui.R")
source("R/server.R")

# Run the app
shinyApp(ui = ui, server = server)
