#====================================================================#
# Author:             Damian Gwozdz (DG)
# Code:               Shiny app for creation of OLS models
# Creation date:      03SEP2017
# Last modified:      23SEP201
# Description:        -
# Required functions: ols
#
#====================================================================#

library(shiny)
source("ols.R")
options(shiny.maxRequestSize=0.05*1024^2)
# install.packages("qdapRegex")
# library(qdapRegex)

ui <- fluidPage(
  titlePanel("Ordinary Least Squares Models in iris data set"),
  sidebarPanel(
    helpText("This application creates an Ordinary Least Squares (OLS) model
             for declared dependent and independent variables from iris dataset.
             The model is tested by multiple statistical tests. Additionally,
             a visualization of predicted vs. observed values is created.",
             strong("Maximum file size was set to 50 kb."),
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
             sliderInput("alpha.bin.slider", "Choose transparence level:",
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
    p("Preview of the frist two rows:"),
    tableOutput("first.two"),
    p(strong("Possible dependent/independent variables:"),
      textOutput("possible.variables")),
    p(strong("Other variables:"), textOutput("other.variables")),
    fluidRow(
      column(4,
             textInput("target.var", h3("Input target variable:"),
                       value = "Sepal.Length")),
      column(8,
             textInput("independent.vars", h3("Input independent variables:"),
                       value = "Petal.Length"))
    ),
    fluidRow(
      column(4,
             textInput("time.var", h3("Input time variable:"), value = ""))
    ),
    plotlyOutput("plot"),
    plotlyOutput("time.plot"),
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
      return(iris)
    }
    
    df <- read.csv(infile$datapath, header = input$header,
                   sep = input$sep,quote = input$quote)
    
    return(df)}
  )
  
  
  # Variables which can be dependent/independent in OLS model:
  output$possible.variables <- reactive(
    names(dset.in())[sapply(dset.in(), is.numeric)]
  )
  # Other variables (including possible time variable):
  output$other.variables <- renderText(
    names(dset.in())[sapply(dset.in(), function(x) !is.numeric(x))]
  )
  
  time.var <- reactive(qdapRegex::rm_white(input$time.var))
  
  
  ignored.time.variables <- c("")

  dset.out <- eventReactive(c(input$time.var, input$file1), {
    
    if(time.var() %in% ignored.time.variables){
      dset.in()
    }else{
      cbind(dset.in(), date.var = 
        as.Date(as.character(dset.in()[,time.var()])))
    }
  })
  
  # Output first two rows:
  first.two <- reactive({
    if(is.null(input$file1)){
      head(iris, 2)
    }else{
      head(dset.out(), 2)
    }
  })
  output$first.two <- renderTable({first.two()})
  
  date.cntrl <- eventReactive(time.var(),{
    if(time.var() %in% ignored.time.variables){
      NULL
    }else{
      "date.var"
    }
  })
  
  model.all <- reactive({
    model <- ols(
      dset = dset.out(),
      target = qdapRegex::rm_white(input$target.var),
      vars = qdapRegex::rm_white(input$independent.vars),
      visualize = T,
      output.residuals = T,
      time.var = date.cntrl())
  })
  
  output$model.stats1 <- renderTable(model.all()[["stats"]][21])
  output$model.stats2 <- renderTable(model.all()[["stats"]][3:10])
  output$model.stats3 <- renderTable(model.all()[["stats"]][11:15])
  output$model.stats4 <- renderTable(model.all()[["stats"]][16:20])
  output$vars.stats <- renderTable(model.all()[["var.stats"]])
  output$plot <- renderPlotly(model.all()[["plot"]])
  output$time.plot <- renderPlotly({
    validate(
      need(date.cntrl(), "No time variale declared: time plot cannot be built.")
    )
    model.all()[["time.plot"]]
  })
  
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
      geom_histogram(aes(x = model.all()[["output.residuals"]]),
                     fill = I(input$histcolour),
                     alpha = I(input$alpha.bin.slider),
                     bins = input$hist.bin.slider) +
      xlab("") +
      ggtitle(paste0("Histogram of residuals: ", normality.test())) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)