#====================================================================#
# Author:             Damian Gwozdz (DG)
# Function:           lags
# Creation date:      31AUG2017
# Last modified:      -
# Description:        Function to add lags of selected variables
#                     to the given dataset
# Required functions: -
#
#====================================================================#

lags <- function(dset, vars, lag.vec){
  
  #====================================================================
  # PARAMETERS:
  #
  # 1) dset - input data set
  # 2) vars - vector of variables names
  # 3) lag.vec - vector of lags
  #
  # EXAMPLE:
  #
  # iris2 <- lags(iris, c("Sepal.Length"), c(3, 5, 7))
  #
  #====================================================================
  
  ## Parameters for tests
  # dset <- iris
  # vars <- c("Sepal.Length")
  # lag.vec <- c(3,5)
  
  for(i in 1:length(vars)){
    for(j in 1:length(lag.vec)){
      dset[[paste0(vars[i], ".", lag.vec[j])]] <- 
        c(rep(NA, times = lag.vec[j]),
          # dset[[vars[i]]][(-(length(dset[[vars[i]]]))+lag.vec[j]-1):-length(dset[[vars[i]]])])
    }
  }
  return(dset)
}

cbind(iris$Sepal.Length, c(NA, NA, iris$Sepal.Length[-149:-150]), 
      c(iris$Sepal.Length - c(NA, NA, iris$Sepal.Length[-149:-150])), 
      c(NA, NA, diff(iris$Sepal.Length, 2)))