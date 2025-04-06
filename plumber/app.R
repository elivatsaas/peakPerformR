# plumber/app.R
library(shiny)
library(plumber)

# Create a Shiny app that hosts the Plumber API
ui <- fluidPage(
  titlePanel("PeakPerformR API"),

  mainPanel(
    h3("API Documentation"),
    p("This is a Plumber API wrapped in a Shiny application."),
    p("Available endpoints:"),
    tags$ul(
      tags$li(strong("GET /"), " - Health check"),
      tags$li(strong("POST /recalculate"), " - Recalculate metrics based on thresholds")
    ),
    p("To use the API endpoints directly, append the endpoint path to the URL.")
  )
)

server <- function(input, output, session) {
  # The server doesn't need to do anything special
  # The Plumber API runs separately
}

# Initialize and run the Plumber API in the background
# This will be executed when the app starts
pr <- plumb("plumber.R")
# Start the API on a different port than Shiny
# Normally you would run this, but shinyapps.io will handle it differently
# pr$run(port = 8000)

# For shinyapps.io deployment, we won't actually run the plumber API directly
# Instead, return the Shiny app
shinyApp(ui = ui, server = server)
