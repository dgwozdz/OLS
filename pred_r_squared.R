#====================================================================#
# Author:             Damian Gwozdz (DG)
# Function:           PRESS
# Creation date:      28JAN2018
# Last modified:      -
# Description:        Function to obtain the PRESS stat
#                     required to compute predicted R-squared
# Required functions: PRESS
# Sources:            1) "Predictive R-squared according to Tom Hopper",
#                       https://rpubs.com/RatherBit/102428
#                       (access: 28JAN2018)
#
#====================================================================#


pred_r_squared <- function(PRESS, model){
  
  #====================================================================
  # PARAMETERS:
  #
  # 1)  PRESS - PRESS stat obtained from PRESS() function
  # 2)  model - Ordinary Least Squared Model created by lm() function
  #====================================================================
  
  avg <- sum(model$model[,1])/length(model$model[,1])
  return( 1 - PRESS/(sum((model$model[,1] - avg)^2)) )
}