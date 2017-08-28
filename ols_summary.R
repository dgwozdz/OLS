ols_summary <- function(dset.sum, target.sum, vars.sum, alpha.sum = .05,
                        intercept.sum = T, do.parallel = F, n.cores = 2,
                        visualize.sum = F){
  
  # Parameters
  dset.sum <- iris
  target.sum <- rep("Sepal.Length", 3)
  vars.sum <- c("Sepal.Width Petal.Length Petal.Width", "Petal.Length Petal.Width",
                "Sepal.Width Petal.Width")
  alpha.sum <- .05
  intercept.sum <- T
  do.parallel <- F
  visualize.sum <- F
  n.cores <- 2
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
  
  if(do.parallel = F){
    for(i in 1:length(vars.sum)){
      ols.i <- ols(dset = dset.sum,
                   target = target.sum[i],
                  vars = vars.sum[i],
                  alpha = alpha.sum,
                  intercept = intercept.sum,
                  visualize = visualize.sum)
      model.stats[i,] <- ols.i[["stats"]]
      model.vars[[i]] <- ols.i[["var.stats"]]
    }
  }else{
    library(parallel)
    cores <- n.cores
    cl <- makeCluster(cores)
    clusterEvalQ(cl, library("caret"))
    clusterEvalQ(cl, library("lmtest"))
    clusterEvalQ(cl, library("nortest"))
    clusterEvalQ(cl, library("car"))
    clusterExport(cl = cl, c("ols", "dset.sum", "target.sum",
                                "vars.sum", "intercept.sum",
                                "alpha.sum", "visualize.sum",
                             "dset", "ols.formula"), envir = .GlobalEnv)
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
}

seq_len(num.models)
