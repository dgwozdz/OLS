#====================================================================#
# Author:             Damian Gwozdz (DG)
# Code:               Shiny app for creation of OLS models
# Creation date:      03SEP201
# Last modified:      -
# Description:        -
# Required functions: ols
#
#====================================================================#

ui <- fluidPage(
  titlePanel("Ordinary Least Squares Model"),
  sidebarPanel(
    helpText("This application creates an Ordinary Least Squares (OLS) model
             for declared dependent and independent variables from iris dataset.
             The model is tested by multiple statistical tests. Additionally,
             a visualization of predicted vs. observed vaues is created.")
    ),
  mainPanel(
    fluidRow(
      column(4,
             textInput("target.var", h3("Target variable:"),
                       value = "Sepal.Length")),
      column(7,
             textInput("independent.vars", h3("Independent variables:"),
                       value = "Sepal.Width"))
    ),
    h3("Model statistics"),
    tableOutput("model.stats1"),
    tableOutput("model.stats2"),
    tableOutput("model.stats3"),
    tableOutput("model.stats4"),
    h3("Variable statistics"),
    tableOutput("vars.stats"),
    plotOutput("plot")
  )
    )

server <- function(input, output){
  model.all <- reactive({
    model <- ols(
      dset = iris,
      target = input$target.var,
      vars = input$independent.vars,
      visualize = F
    )
    model.stats <- model[["stats"]]
    model.vars.stats <- model[["var.stats"]]
    model.plot <- model[["plot"]]
    list(stats = model.stats,
         vars.stats = model.vars.stats,
         plot = model.plot)
  })
  output$model.stats1 <- renderTable({model.all()[["stats"]][20]})
  output$model.stats2 <- renderTable({model.all()[["stats"]][3:10]})
  output$model.stats3 <- renderTable({model.all()[["stats"]][11:17]})
  output$model.stats4 <- renderTable({model.all()[["stats"]][16:19]})
  output$vars.stats <- renderTable({model.all()[["vars.stats"]]})
  output$plot <- renderPlot({model.all()[["plot"]]})
}

shinyApp(ui = ui, server = server)