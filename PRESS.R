#====================================================================#
# Author:             Damian Gwozdz (DG)
# Function:           PRESS
# Creation date:      28JAN2018
# Last modified:      -
# Description:        Function to compute the PRESS stat
#                     required to calculate predicted R-squared
#
# Sources:            1) "Predictive R-squared according to Tom Hopper",
#                       https://rpubs.com/RatherBit/102428
#                       (access: 28JAN2018)

#
#====================================================================#

PRESS <- function(dset, target, vars, intercept = TRUE){
  
  #====================================================================
  # PARAMETERS:
  #
  # 1)  dset - input data set
  # 2)  target - target variable declared as a string
  # 3)  vars - independent variables declared as a string
  #             with blanks as separators
  # 4)  intercept - a boolean value indicating whether the built model
  #               should have an intercept
  #====================================================================
  
  n <- nrow(dset)
  ones <- rep(1, nrow(dset)-1)
  specpr<-numeric(n)
  vars.split <- unlist(strsplit(vars, " "))
  
  if(intercept){
    for(i in 1:nrow(dset)){
      
      # If intercept, the column of size n x 1 (n is the number of
      # observations in the dset): `ones` must be added
      
      model<-fastLmPure(as.matrix(cbind(ones, dset[-i, vars.split])),
                        dset[[target]][-i])
      specpr[i]<-sum(model$coefficients*unlist(c(1, dset[i, vars.split])))
    }
    
  }else{
    
    for(i in 1:nrow(dset)){
      
      model<-fastLmPure(as.matrix(dset[-i, vars.split]),
                        dset[[target]][-i])
      specpr[i]<-sum(model$coefficients*unlist(dset[i, vars.split]))
    }
  }
  
  
  return( sum((dset[[target]]-specpr)^2) )
}