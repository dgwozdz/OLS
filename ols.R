library(lmtest)
# install.packages("nortest")
library(nortest) # Anderson-Darling test
library(car) # VIF
library(caret) # RMSE


ols <- function(dset, target, vars, alpha = .05, intercept = T, visualize = F){
  
  # parameters
  dset <- iris
  target <- "Sepal.Length"
  vars <- "Sepal.Width Petal.Length Petal.Width"
  alpha <- .05
  intercept <- T
  visualize <- T
  
  vars.split <- unlist(strsplit(vars, " "))
  
  nvars <- if(intercept == T){length(vars.split)+1}else{length(vars.split)}
  
  dset <- dset[,c(target, vars.split)]
  ols.formula <- paste0(target, "~", gsub(" ", "+", vars), if(intercept == T){""}else{"-1"})
  model.original <- lm(formula = ols.formula, data = dset)
  model <- summary(model.original)
  
  # Model stats
  
  model.stats <- data.frame(target = NA, vars = NA, R2 = NA,
                            adjusted.R2 = NA, RMSE = NA, F.stat = NA, F.p.value = NA,
                            bp.stat = NA, bp.p.value = NA, bg.stat = NA,
                            bg.p.value = NA, reset.stat = NA,
                            reset.p.value = NA, ad.stat = NA, ad.p.value = NA,
                            sw.stat = NA, sw.p.value = NA, max.vif = NA, tests = NA,
                            equation = NA)
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
  
  vifs <- vif(model.original)
  if(intercept == T){model.vars$vif <- c(NA, vifs)}else
  {model.vars$vif <- vifs}
  
  model.stats$max.vif <- if(length(vars.split) == 1)NA else max(model.vars$vif, na.rm = T)
  
  model.stats$tests <- if(model.stats$bp.p.value<alpha &
                           model.stats$bg.p.value<alpha &
                           model.stats$reset.p.value<alpha &
                           model.stats$ad.p.value<alpha &
                           model.stats$sw.p.value<alpha)T else F
  model.stats$equation <- paste0(paste0(as.character(model.vars$var), sep = "*"),
                                 paste0("(", model.vars$coef , ")"), collapse = "+")
  return(list(stats = model.stats, var.stats = model.vars))
}

# test

# parameters
# dset <- iris
# target <- "Sepal.Length"
# vars <- "Sepal.Width Petal.Length Petal.Width"
# alpha <- .05
# intercept <- T
# visualize <- T


model <- ols(dset = iris,
    target = "Sepal.Length",
    vars = "Sepal.Width Petal.Length Petal.Width")

length(model[["stats"]])
