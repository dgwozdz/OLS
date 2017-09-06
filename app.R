#====================================================================#
# Author:             Damian Gwozdz (DG)
# Code:               Shiny app for creation of OLS models
# Creation date:      03SEP201
# Last modified:      -
# Description:        -
# Required functions: ols
#
#====================================================================#
library(shiny)
source("ols.R")s
options(shiny.maxRequestSize=0.05*1024^2)

ui <- fluidPage(
  titlePanel("Ordinary Least Squares Models in iris data set"),
  sidebarPanel(
    helpText("This application creates an Ordinary Least Squares (OLS) model
             for declared dependent and independent variables from iris dataset.
             The model is tested by multiple statistical tests. Additionally,
             a visualization of predicted vs. observed values is created.",
             br(),
             br(),
             strong("Variables in 'Independent Variables' section should 
                    be separated by spaces."),
             br(),
             br(),
             "Explanations of acronyms:",
             tags$ul(
               tags$li(strong("tests:"), "does model comply with all the proposed
                       tests?"),
               tags$li(strong("F:"),"F test for siginificance of all variables"), 
               tags$li(strong("bp:"), "Breusch-Pagan test for homoscedasticity"), 
               tags$li(strong("bg:"), "Breusch-Godfrey test for autocorellation"), 
               tags$li(strong("RESET:"), "RESET test checking whether the
                       linear model function is correct"),
               tags$li(strong("ad:"), "Adnderson-Darling test for normal
                       distribution of error term"),
               tags$li(strong("VIF:"), "Variance Inflation Factor"),
               tags$li(strong("max.vif:"), "Maximum VIF in the model"),
               tags$li(strong("sw:"), "Shapiro-Wilk test for normal distribution
                       of error term"),
               tags$li(strong("n:"), "number of observations in the 
                       input data set")
               ),
             
             selectInput("histcolour", "Choose histogram colour:",
                         choices = c("red", "green", "blue", "black",
                                     "cyan", "violet", "purple", "black",
                                     "yellow", "grey"), selected = "blue"),
             sliderInput("hist.bin.slider", "Choose number of bins of histogram:",
                         min = 1, max = nrow(iris), value = 30, step = 5),
             sliderInput("alpha.bin.slider", "Choose transaparence:",
                         min = 0, max = 1, value = 0.7, step = 0.1)
               )
             ),
  mainPanel(
    fluidRow(
      column(5,
             fileInput('file1', "Input a csv file:"),
             accept=c('text/csv', 
                      'text/comma-separated-values,text/plain', 
                      '.csv'),
             checkboxInput('header', 'Header', TRUE)),
      column(3,
             radioButtons('sep', 'Separator',
                          c(Comma=',',
                            Semicolon=';',
                            Tab='\t'),
                          ',')
      ),
      column(3,
             radioButtons('quote', 'Quote',
                          c(None='',
                            'Double Quote'='"',
                            'Single Quote'="'"),
                          '"')
      )
    ),
    p(strong("Variables to choose from:"), textOutput("possible.variables")),
    fluidRow(
      column(4,
             textInput("target.var", h3("Input target variable:"),
                       value = "Target variable here")),
      column(8,
             textInput("independent.vars", h3("Input independent variables:"),
                       value = "Independent variables separated by blanks here"))
    ),
    plotlyOutput("plot"),
    plotlyOutput("histogram.residuals"),
    h3("Model statistics"),
    tableOutput("model.stats1"),
    tableOutput("model.stats2"),
    tableOutput("model.stats3"),
    tableOutput("model.stats4"),
    h3("Variable statistics"),
    tableOutput("vars.stats")
  )
)

server <- function(input, output){
  dset.in <- reactive({
    infile <- input$file1
    if(is.null(infile)){
      return(NULL)
    }
    
    df <- read.csv(infile$datapath, header = input$header,
                   sep = input$sep,quote = input$quote)
    return(df)}
  )
  
  output$possible.variables <- renderText(
    names(dset.in())[sapply(dset.in(), is.numeric)]
  )
  
  model.all <- reactive({
    model <- ols(
      dset = dset.in(),
      target = input$target.var,
      vars = input$independent.vars,
      visualize = T,
      output.residuals = T
    )
    model.stats <- model[["stats"]]
    model.vars.stats <- model[["var.stats"]]
    model.plot <- ggplotly(model[["plot"]])
    model.residuals <- model[["output.residuals"]]
    list(stats = model.stats,
         vars.stats = model.vars.stats,
         plot = model.plot,
         residuals = model.residuals)
  })
  output$model.stats1 <- renderTable({model.all()[["stats"]][21]})
  output$model.stats2 <- renderTable({model.all()[["stats"]][3:10]})
  output$model.stats3 <- renderTable({model.all()[["stats"]][11:15]})
  output$model.stats4 <- renderTable({model.all()[["stats"]][16:20]})
  output$vars.stats <- renderTable({model.all()[["vars.stats"]]})
  output$plot <- renderPlotly({model.all()[["plot"]]})
  
  
  # Evaluate H0 of normality tests for different tests:
  # - n<30: Shapiro-Wilk
  # - n>=30: Anderson-Darling
  
  normality.test <- reactive(
    if(model.all()[["stats"]]$n<30){
      if(model.all()[["stats"]]$sw.p.value<0.05){
        "distribution other than normal"
      }else{
        "normal distribution"
      }
    }else{
      if(model.all()[["stats"]]$ad.p.value<0.05){
        "distribution other than normal"
      }else{
        "normal distribution"
      }
    }
  )
  
  output$histogram.residuals <- renderPlotly({
    ggplot() +
      geom_histogram(aes(x = model.all()[["residuals"]]),
                     fill = I(input$histcolour),
                     alpha = I(input$alpha.bin.slider),
                     bins = input$hist.bin.slider) +
      xlab("") +
      ggtitle(paste0("Histogram of residuals: ", normality.test())) +
      theme_minimal()
  })
  
}

shinyApp(ui = ui, server = server)