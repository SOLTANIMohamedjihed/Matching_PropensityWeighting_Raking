library(shiny)
library(MatchIt)
library(WeightIt)
library(survey)

# UI
ui <- fluidPage(
  titlePanel("Matching, Propensity weighting, and Raking"),
  sidebarLayout(
    sidebarPanel(
      fileInput("data_file", "Import data file (CSV format)"),
      selectInput("analysis_type", "Choose analysis type:",
                  choices = c("Matching", "Propensity weighting", "Raking"))
    ),
    mainPanel(
      # Output for displaying results
      verbatimTextOutput("results_output")
    )
  )
)

# Server
server <- function(input, output) {

  # Read data
  data <- reactive({
    req(input$data_file)
    read.csv(input$data_file$datapath)
  })

  # Perform analysis based on selected type
  output$results_output <- renderPrint({
    req(data())

    if (input$analysis_type == "Matching") {
      # Perform matching
      matched_data <- matchit(treatment ~ covariate1 + covariate2, data = data())
      summary(matched_data)
    } else if (input$analysis_type == "Propensity weighting") {
      # Perform propensity weighting
      weighted_data <- weightit(treatment ~ covariate1 + covariate2, data = data(),
                                method = "ps", estimand = "ATE")
      summary(weighted_data)
    } else if (input$analysis_type == "Raking") {
      # Perform raking
      raked_data <- rake(design = svydesign(ids = ~1, weights = ~weights, data = data()),
                         sample.margins = c("margin1", "margin2"),
                         population.margins = c("pop_margin1", "pop_margin2"))
      summary(raked_data)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)