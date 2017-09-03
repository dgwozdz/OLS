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
      column(4.5,
             textInput("target.var", h3("Input target variable:"),
              value = "Sepal.Length")),
    column(7,
           textInput("independent.vars", h3("Input independent variables:"),
              value = "Sepal.Width")),
    tableOutput("model.stats"),
    tableOutput("vars.stats"),
    plotOutput("plot")
    )
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
   output$model.stats <- renderTable(model.all()[["stats"]])
   output$vars.stats <- renderTable(model.all()[["vars.stats"]])
   output$plot <- renderPlot(model.all()[["plot"]])
}

shinyApp(ui = ui, server = server)