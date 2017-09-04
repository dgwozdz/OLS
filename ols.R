#====================================================================#
# Author:             Damian Gwozdz (DG)
# Function:           ols
# Creation date:      15JUN2017
# Last modified:      03SEP2017
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
library(plotly)


ols <- function(dset, target, vars, alpha = .05, intercept = T,
                visualize = F, output.residuals = F){
  
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
  #====================================================================
  
  ## parameters
  # dset <- iris
  # target <- "Sepal.Length"
  # vars <- "Sepal.Width Petal.Width"
  # alpha <- .05
  # intercept <- T
  # visualize <- T
  # output.errors <- T
  
  vars.split <- unlist(strsplit(vars, " "))
  
  nvars <- if(intercept == T){length(vars.split)+1}else{length(vars.split)}
  
  dset <- dset[,c(target, vars.split)]
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
                            sw.stat = NA, sw.p.value = NA, max.vif = NA,
                            tests = NA, n = NA, equation = NA)
  model.stats$target <- target
  model.stats$vars <- vars
  model.stats$R2 <- model$r.squared
  model.stats$adjusted.R2 <- model$adj.r.squared
  model.stats$RMSE <- RMSE(predict(model.original), dset[[target]])
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
                           model.stats$sw.p.value<alpha)T else F
  model.stats$n <- nrow(dset)
  model.stats$equation <- paste0(paste0(as.character(model.vars$var), sep = "*"),
                                 paste0("(", model.vars$coef , ")"), collapse = "+")
  
  if(visualize == T){
    dset$predicted <- predict(model.original, dset)
    model.plot <- ggplot(dset, aes_string(x="predicted", y=target)) +
      geom_point(shape=19, color = "purple") +
      xlab("Predicted") +
      ylab("Observed") +
      ggtitle(paste0(target, ": Predicted vs Observed, Adj. R2=",
                     percent(model.stats$adjusted.R2))) +
      theme_minimal()
    # print(model.plot)
  }
  
  if(output.residuals == T){
    model.errors <- model$residuals
  }
  
  return(list(stats = model.stats, var.stats = model.vars, plot = model.plot,
              output.residuals = model$residuals))
}


# model <- ols(dset = iris,
#     target = "Sepal.Length",
#     vars = "Sepal.Width",
#     visualize = T, output.residuals = T)
# model[["plot"]]
