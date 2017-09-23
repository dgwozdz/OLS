#====================================================================#
# Author:             Damian Gwozdz (DG)
# Function:           ols
# Creation date:      15JUN2017
# Last modified:      15SEP2017
# Description:        Function to build an Ordinary
#                     Least Squares models and test it
# Required functions: -
#
#   Utilized tests:
#     1) Breusch-Pagan (heteroscedasticity)
#     2) Breusch-Godfrey (serial autocorrelation)
#     3) RESET
#     4) Anderson-Darling (normality of error distribution)
#     5) Shapiro-Wilk (normality of error distribution)
#     6) Chow test (time-series stability)
#
#   Visualizations:
#     1) Predicted vs. Observed
#
#====================================================================#

library(lmtest)
# install.packages("nortest")
library(nortest) # Anderson-Darling test
library(car) # VIF
library(caret) # RMSE
library(scales) # percent() function
library(plotly) # interactive predicted vs. observed plot
# install.packages("strucchange")
library(strucchange) # chow test
# install.packages("lubridate")
library(lubridate)


ols <- function(dset, target, vars, alpha = .05, intercept = T,
                visualize = F, output.residuals = F,
                time.series = F, time.var = NULL){
  
  #====================================================================
  # PARAMETERS:
  #
  # 1)  dset - input data set
  # 2)  target - target variable declared as a string
  # 3)  vars - indepependent variables declared as a string
  #             with blanks as separators
  # 4)  alpha - significance level
  # 5)  intercept - a boolean value indicating whether the built model
  #               should have an intercept
  # 6)  visualize  - a boolean value indicating whether the built model
  #               should be visualized (plot: predicted vs. observed)
  # 7)  output.residuals  - a boolean value indicating whether the error
  #                   term should be saved
  # 8)  time.series - a boolean value indicating the name of the variable
  #                 which indicates time
  # 9)  time.var - variable identifying time
  #====================================================================
  
  ## parameters
  # dset <- iris
  # target <- "Sepal.Length"
  # vars <- "Sepal.Width Petal.Width"
  # time.var <- NULL
  # dset <- EuStockMarkets
  # target <- "DAX"
  # vars <- "FTSE CAC"
  # alpha <- .05
  # intercept <- T
  # visualize <- T
  # output.residuals <- T
  # time.series <- F
  # time.var <- NULL
  # dset <- EuStockMarkets2
  

  
  # If a ts object is declared as an input data set, transform it
  # to a data frame
  
  if(sum(class(dset) == "ts")>0){
    dset <- data.frame(as.matrix(dset),
                       date.custom=as.yearmon(time(dset)))
  }else if(length(time.var)>0 & class(dset[, time.var]) != "Date"){
    dset$date.custom <- date_decimal(dset[,time.var])
  }
  # else{
  #   stop("Declared data set is not an object of class 'ts' or
  #        the time variable was not declared")
  # }
  
  vars.split <- unlist(strsplit(vars, " "))
  
  nvars <- if(intercept == T){length(vars.split)+1}else{length(vars.split)}
  
  if(time.series == T){
    dset <- dset[,c(target, vars.split, "date.custom")]
  }else if(!is.null(time.var)){
    dset <- dset[,c(target, vars.split, time.var)]
  }else{
    dset <- dset[,c(target, vars.split)]
  }
  
  intercept.string <- if(intercept == T){""}else{"-1"}
  ols.formula <- as.formula(paste0(target, "~", gsub(" ", "+", vars), intercept.string))
  model.original <- lm(formula = ols.formula, data = dset)
  model <- summary(model.original)
  
  # Model stats
  
  model.stats <- data.frame(target = NA, vars = NA, R2 = NA,
                            adjusted.R2 = NA, RMSE = NA, F.stat = NA, F.p.value = NA,
                            bp.stat = NA, bp.p.value = NA, bg.stat = NA,
                            bg.p.value = NA, reset.stat = NA,
                            reset.p.value = NA, ad.stat = NA, ad.p.value = NA,
                            sw.stat = NA, sw.p.value = NA,
                            chow.stat = NA, chow.p.value = NA,
                            max.vif = NA,
                            tests = NA, n = NA, equation = NA)
  model.stats$target <- target
  model.stats$vars <- vars
  model.stats$R2 <- model$r.squared
  model.stats$adjusted.R2 <- model$adj.r.squared
  model.stats$RMSE <- RMSE(predict(model.original),
                           # both: target and predicted value must be available to
                           # reliably compute RMSE
                           dset[apply(dset, 1, function(x) !sum(is.na(x))),
                                c(target, vars.split)])
  model.stats$F.stat <- model$fstatistic["value"]
  model.stats$F.p.value <- pf(model$fstatistic[1], model$fstatistic[2],
                              model$fstatistic[3], lower=FALSE)
  
  # Model tests
  
  breusch.pagan <- bptest(model)
  model.stats$bp.stat <- breusch.pagan$statistic
  model.stats$bp.p.value <- breusch.pagan$p.value
  
  breusch.godfrey <- bgtest(model)
  model.stats$bg.stat <- breusch.godfrey$statistic
  model.stats$bg.p.value <- breusch.godfrey$p.value
  
  reset <- resettest(model)
  model.stats$reset.stat <- reset$statistic
  model.stats$reset.p.value <- reset$p.value
  
  anderson.darling <- ad.test(model$residuals)
  model.stats$ad.stat <- anderson.darling$statistic
  model.stats$ad.p.value <- anderson.darling$p.value
  
  shapiro.wilk <- shapiro.test(model$residuals)
  model.stats$sw.stat <- shapiro.wilk$statistic
  model.stats$sw.p.value <- shapiro.wilk$p.value
  
  chow <- sctest(ols.formula, type = "Chow", data = dset)
  model.stats$chow.stat <- chow$statistic
  model.stats$chow.p.value <- chow$p.value
  
  # Variable stats & tests
  
  model.vars <- data.frame(var = rownames(model$coefficients),
                           coef = model$coefficients[,"Estimate"],
                           p.value = model$coefficients[,4], vif = rep(NA, nvars))
  
  if(length(vars.split) == 1){
    if(intercept == T){
      model.vars$vif <- c(rep(NA, 2))
    }else{
      model.vars$vif <- NA
    }
  }else{
    if(intercept == T){
      model.vars$vif <- c(NA, vif(model.original))
    }else{
      model.vars$vif <- vif(model.original)
    }
  }
  
  model.stats$max.vif <- if(length(vars.split) == 1)NA else max(model.vars$vif, na.rm = T)
  
  model.stats$tests <- if(model.stats$bp.p.value<alpha &
                           model.stats$bg.p.value<alpha &
                           model.stats$reset.p.value<alpha &
                           model.stats$ad.p.value<alpha &
                           model.stats$sw.p.value<alpha &
                          model.stats$chow.p.value<alpha)T else F
  model.stats$n <- nrow(dset)
  model.stats$equation <- paste0(paste0(as.character(model.vars$var), sep = "*"),
                                 paste0("(", model.vars$coef , ")"), collapse = "+")
  
  if(visualize == T){
    
    dset$predicted <- predict(model.original, dset)
    
    if(time.series == T | length(time.var)>0){
      model.plot <- ggplot(dset, aes_string(x="predicted", y=target)) +
        geom_point(shape=19, color = "purple") +
        xlab("Predicted") +
        ylab("Observed") +
        ggtitle(paste0(target, ": Predicted vs Observed, Adj. R2=",
                       percent(model.stats$adjusted.R2))) +
        theme_minimal()
    }
    if(time.series == T){
      time.series.plot <- ggplot() +
        geom_line(data = dset, aes_string(x="date.custom", y=target,
                                          col = "target")) +
        geom_line(data = dset, aes(x=date.custom, y=predicted,
                                   col = paste0("predicted ", target))) +
        xlab("Time") +
        ylab(target) +
        ggtitle(paste0(target, ": Predicted vs Observed")) +
        labs(color = "") +
        theme_minimal()
    }else if(length(time.var)>0){
      dset_ggplot <- reshape2::melt(dset[,c(target, "predicted", time.var)],
                                    id = time.var)
      time.series.plot <- ggplot(data=dset_ggplot,
             aes_string(x = time.var, y = "value",
                        colour = "variable", group = "variable")) +
        geom_line() +
        xlab("Time") +
        ylab(target) +
        ggtitle(paste0(target, ": Predicted vs Observed")) +
        labs(color = "") +
        theme_minimal()
    }else{
      
      # dset$predicted <- predict(model.original, dset)
      model.plot <- ggplot(dset, aes_string(x="predicted", y=target)) +
        geom_point(shape=19, color = "purple") +
        xlab("Predicted") +
        ylab("Observed") +
        ggtitle(paste0(target, ": Predicted vs Observed, Adj. R2=",
                       percent(model.stats$adjusted.R2))) +
        theme_minimal()
      # print(model.plot)
      time.series.plot<- NULL
    }
  }else{
    model.plot <- NULL
    time.series.plot<- NULL
  }
  
  if(output.residuals == T){
    model.errors <- model$residuals
  }else{
    model.errors <- NULL
  }
  
  return(list(stats = model.stats, var.stats = model.vars, plot = model.plot,
                output.residuals = model.errors,
                time.plot = time.series.plot))
}

### Examples
# model <- ols(dset = iris,
#     target = "Sepal.Length",
#     vars = "Sepal.Width",
#     visualize = T, output.residuals = T, time.series = F)

# EuStockMarkets2 <- data.frame(as.matrix(EuStockMarkets),
#                           date=as.yearmon(time(EuStockMarkets)))
# 
# model <- ols(dset = EuStockMarkets2,
#              target = "DAX",
#              vars = "FTSE CAC",
#              visualize = T, output.residuals = T, time.series = F,
#              time.var = NULL)
# model[["plot"]]

# a <- read.csv("EuStockMarkets2.csv")
# ols(dset = EuStockMarkets2,
#                  target = "DAX",
#                  vars = "FTSE CAC",
#                  visualize = T, output.residuals = T,
#                  time.var = "NULL")