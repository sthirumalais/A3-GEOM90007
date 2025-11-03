# Main App File
# Sources all modules and runs the Shiny app

## The project idea and implementation are original. 
## Generative AI tools were used only for code review, optimization, 
## and visual refinement — strictly within the 
## University’s academic integrity guidelines.

source("R/libraries.R")

source("R/tableau-in-shiny-v1.2.R")
source("R/data.R")
source("R/map.R")
source("R/ui.R")
source("R/server.R")

# Expose static assets (CSS/JS/images)
shiny::addResourcePath("assets", "www")

# Expose Data directory for images/audio in modals
shiny::addResourcePath("bird-data", "Data")

app <- shinyApp(ui = ui, server = server)

# Launch in external browser
runApp(app, launch.browser = TRUE)
