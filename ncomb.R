#====================================================================#
# Author:             Damian Gwozdz (DG)
# Function:           combs
# Creation date:      30AUG2017
# Last modified:      30AUG2017
# Description:        Function to create all possible combinations
#                     from a given vector of strings including their
#                     lags
# Required functions: ols
#
#====================================================================#


ncomb <- function(vec, m = 1, n, max.lag){
  
  # vec <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j')
  # max.lag <- 4
  # n <- 7
  # m <- 1
  
  try(if(n>length(vec))
    stop("Number of combinations is greater than the number of the elements in the declared vector."))
  
  combs <- c()
  for(i in m:n){
    combs <-c(combs,
              sapply(combn(vec, i, simplify = F), paste0, collapse = " "))
  }
  
  
  combs.lags <- c()
  for(j in 1:length(combs)){
    separate.strings <- unlist(strsplit(combs[j], " "))
    matrix.string <- rbind(sapply(separate.strings, paste0, ".", 1:max.lag),
                           separate.strings)
    matrix.comb <- expand.grid(lapply(seq_len(ncol(matrix.string)), function(i) matrix.string[,i]))
    combs.lags<- append(combs.lags, apply(matrix.comb, 1, paste0, collapse = " "))
  }
  
  return(combs.lags)
}

# combs <- ncomb(vec = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'),
#       m = 1,
#       n = 4,
#       max.lag = 2)