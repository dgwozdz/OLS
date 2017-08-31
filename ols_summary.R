#====================================================================#
# Author:             Damian Gwozdz (DG)
# Function:           ols
# Creation date:      15JUN2017
# Last modified:      30AUG201
# Description:        Function to build multiple Ordinary
#                     Least Squares models in a parallelized way
# Required functions: ols
#
#====================================================================#

rm(list = ls())
source("ols.r")
source("ncomb.r")

ols_summary <- function(dset.sum, target.sum, vars.sum, alpha.sum = .05,
                        intercept.sum = T, do.parallel = F, n.cores = 2,
                        visualize.sum = F){
  
  #====================================================================
  # PARAMETERS:
  #
  # 1) dset.sum - input data set
  # 2) target.sum - vector of target variables declared as strings
  # 3) vars.sum - vector of independent variables declared as strings
  #               and separated by blanks in each string
  # 4) alpha.sum - significance level
  # 5) intercept.sum - a boolean value indicating whether the built model
  #               should have an intercept
  # 6) do.parallel - a boolean value indicating whether parallelization
  #               should be used
  # 7) n.cores - number of utilized cores (active only if do.parallel == T)
  # 6) visualize.sum  - a boolean value indicating whether the built model
  #               should be visualized [CURRENTLY INACTIVE]
  #====================================================================
  
  # Parameters
  # dset.sum <- iris
  # target.sum <- rep("Sepal.Length", 3)
  # vars.sum <- c("Sepal.Width Petal.Length Petal.Width", "Petal.Length Petal.Width",
  #               "Sepal.Width Petal.Width")
  # alpha.sum <- .05
  # intercept.sum <- T
  # do.parallel <- F
  # visualize.sum <- F
  # n.cores <- 2

  
  #
  num.models <- length(vars.sum)
  num.NA <- rep(NA, num.models)
  
  
  model.stats <- data.frame(target = num.NA, vars = num.NA, R2 = num.NA,
                            adjusted.R2 = num.NA, RMSE = num.NA, F.stat = num.NA,
                            F.p.value = num.NA, bp.stat = num.NA,
                            bp.p.value = num.NA, bg.stat = num.NA,
                            bg.p.value = num.NA, reset.stat = num.NA,
                            reset.p.value = num.NA, ad.stat = num.NA,
                            ad.p.value = num.NA, sw.stat = num.NA,
                            sw.p.value = num.NA, max.vif = num.NA,
                            tests = num.NA, equation = num.NA)
  model.vars <- vector(mode = "list", length = num.models)
  
  
  # Non-parallelized version
  if(do.parallel == F){
    for(i in 1:length(vars.sum)){
      ols.i <- ols(dset = dset.sum,
                   target = target.sum[i],
                  vars = vars.sum[i],
                  alpha = alpha.sum,
                  intercept = intercept.sum,
                  visualize = visualize.sum)
      model.stats[i,] <- ols.i[["stats"]]
      model.vars[[i]] <- ols.i[["var.stats"]]
      models <- list(model.stats, model.vars)
    }
    
  # Parallelized version
  }else{
    library(parallel)
    cores <- n.cores
    cl <- makeCluster(cores)
    clusterEvalQ(cl, library("caret"))
    clusterEvalQ(cl, library("lmtest"))
    clusterEvalQ(cl, library("nortest"))
    clusterEvalQ(cl, library("car"))
    clusterExport(cl = cl,
                  varlist = c("ols",
                              "dset.sum", "vars.sum", "target.sum",
                              "alpha.sum", "intercept.sum", "visualize.sum"),
                  envir = environment())
    models <- parLapply(cl = cl, X = seq_len(num.models),
              function(i) ols(dset = dset.sum, 
                           target = target.sum[i],
                           vars = vars.sum[i],
                           alpha = alpha.sum,
                           intercept = intercept.sum,
                           visualize = visualize.sum)
              )
    stopCluster(cl) 
  }
  return(models)
}

# seq_len(num.models)

# target.all <- rep("Sepal.Length", 
#                   length(
#                     ncomb(vec = c('Sepal.Width', 'Petal.Length'),
#                           m = 1,
#                           n = 3,
#                           max.lag = 0)
#                   ))
# vars.all <- ncomb(vec = c('Sepal.Width', 'Petal.Length'),
#                   m = 1,
#                   n = 3,
#                   max.lag = 0)
# 
# system.time(
# all <- ols_summary(dset.sum = iris,
#             target.sum = target.all,
#             vars.sum = vars.all,
#             alpha.sum = .05,
#             intercept.sum = T,
#             do.parallel = F,
#             visualize.sum = F,
#             n.cores = 2))
