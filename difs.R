#====================================================================#
# Author:             Damian Gwozdz (DG)
# Function:           difs
# Creation date:      31AUG2017
# Last modified:      -
# Description:        Function to add differences of selected variables
#                     to the given dataset
# Required functions: -
#
#====================================================================#

difs <- function(dset, vars, dif.vec){
  
  #====================================================================
  # PARAMETERS:
  #
  # 1) dset - input data set
  # 2) vars - vector of variables names
  # 3) dif.vec - vector of differences
  #
  # EXAMPLE:
  #
  # iris2 <- difs(iris, c("Sepal.Length"), c(3, 5, 7))
  #
  #====================================================================
  
  ## Parameters for tests
  # dset <- iris
  # vars <- c("Sepal.Length")
  # dif.vec <- c(3,5)
  
  for(i in 1:length(vars)){
    for(j in 1:length(lag.vec)){
      dset[[paste0("Diff.", vars[i], ".", dif.vec[j])]] <-
        c(rep(NA, dif.vec[j]), diff(dset[[vars[i]]], dif.vec[j]))
    }
  }
  return(dset)
}

difs(iris, c("Sepal.Length"), c(3, 5, 7))
