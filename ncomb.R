#====================================================================#
# Author:             Damian Gwozdz (DG)
# Function:           combs
# Creation date:      30AUG2017
# Last modified:      14MAY2018
# Description:        Function to create all possible combinations
#                     from a given vector of strings including their
#                     lags
# Required functions: -
#
#====================================================================#


ncomb <- function(vec, m = 1, n, max.lag){
  
  #====================================================================
  # PARAMETERS:
  #
  # 1) vec - vector of input variables (names as strings)
  # 2) m - minimum number of combinations
  # 3) n - maximum number of combinations
  # 4) max.lag - maximum lag (from 0 to max.lag). WARNING:
  #     max.lag set to 0 does not create lagged variables
  #
  # EXAMPLE:
  #
  # combs <- ncomb(vec = c('Diff.a.1', 'b', 'c', 'd',
  # 'Rel.diff.e.1', 'f', 'g', 'h', 'i', 'j'),
  #                m = 1,
  #                n = 3,
  #                max.lag = 2)
  #====================================================================
  
  ## Parameters for tests
  # vec <- c('Diff.a.1', 'b', 'Rel.diff.c.1', 'd', 'e', 'f', 'g', 'h', 'i', 'j')
  # max.lag <- 4
  # n <- 4
  # m <- 1
  
  ### Error - the number of variables from which the combination should
  # be built is larger than the number of variables
  
  try(if(n>length(vec))
    stop("Number of combinations is greater than the number of the
         elements in the declared vector."))
  
  ### Removing the number identifying differencing from 
  # variable names
  
  cut.diff.order<- function(vect){
    
    # Function to cut the number identifying differencing from 
    # the variable name
    # Example:
    #
    # If variable name is "Diff.GDP.3" then the function returns
    # "Diff.GDP"

    vect <- if(grepl('Diff.', vect)){
      c(substring(vect, 1,
                  which(unlist(strsplit(vect, "")) == ".")[
                    length(which(unlist(strsplit(vect, "")) == "."))
                  ]-1))
      }else if(grepl('Rel.diff.', vect)){
      c(substring(vect, 1,
                  which(unlist(strsplit(vect, "")) == ".")[
                    length(which(unlist(strsplit(vect, "")) == "."))
                  ]-1))
      }else{vect}
    
    return(vect)
  }
  vec <- vapply(vec, cut.diff.order, "", USE.NAMES = FALSE)
  
  ## All combinations without lags

  combs <- c()
  for(i in m:n){
    combs <-c(combs,
              sapply(combn(vec, i, simplify = FALSE), paste0, collapse = " "))
  }
  
  if(max.lag == 0){
    return(combs)
  }else{
    
    ## All combinations with lags
    
    combs.lags <- c()
    for(j in 1:length(combs)){
      separate.strings <- unlist(strsplit(combs[j], " "))
      
      # If any variable is a difference, do not add this variable
      # (i.e. if the variable "Diff.GDP" is present, it won't
      # be added: only versions with numbers at the end will
      # be added)
      
      matrix.string <- if(sum(grepl("Diff.", vec))>0){
        sapply(separate.strings, paste0, ".", 1:max.lag)
      }else{
        rbind(sapply(separate.strings, paste0, ".", 1:max.lag),
              separate.strings)
      }
      
      matrix.comb <- expand.grid(lapply(seq_len(ncol(matrix.string)), function(i) matrix.string[,i]))
      combs.lags<- append(combs.lags, apply(matrix.comb, 1, paste0, collapse = " "))
      # print(j)
    }
    
    return(combs.lags)
  }
  
}

# combs <- ncomb(vec = c('Diff.a.1', 'b', 'c', 'd',
#                        'Rel.diff.e.1', 'f', 'g', 'h', 'i', 'j'),
#       m = 1,
#       n = 4,
#       max.lag = 4)
# 
# ncomb(vec = c('Sepal.Width', 'Petal.Length',
#               'Petal.Width'),
#       m = 1,
#       n = 3,
#       max.lag = 0)