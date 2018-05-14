#====================================================================#
# Author:             Damian Gwozdz (DG)
# Function:           -
# Creation date:      21JAN2018
# Last modified:      -
# Description:        Example usage of functions for building
#                     multiple OLS models and visualizations
# Required functions: ols, ols_summary, lags, difs
#
#====================================================================#

setwd('..')

library(datasets)
library(utils)
library(RcppEigen)
data(EuStockMarkets)

functions <- c("PRESS.R", "pred_r_squared.R",
               "ols.R", "ols_summary.R", "ncomb.R",
               "lags.R", "difs.R")
sapply(functions, source)
model1 <- ols(dset = EuStockMarkets,
              target = "DAX",
              vars = "SMI CAC FTSE")
model1[["stats"]]
model1[["var.stats"]]

# Plot
model1 <- ols(dset = EuStockMarkets,
              target = "DAX",
              vars = "SMI CAC FTSE",
              visualize = T)
model1[["plot"]] # plot

# Residuals
model1 <- ols(dset = EuStockMarkets,
                target = "DAX",
                vars = "SMI CAC FTSE",
                visualize = TRUE,
                output.residuals = TRUE)
qplot(model1[["output.residuals"]]) +
  theme_minimal() +
  xlab('') +
  ggtitle("Residuals") # plot

# Change p-value

model1 <- ols(dset = EuStockMarkets,
              target = "DAX",
              vars = "SMI CAC FTSE",
              time.series  = TRUE,
              alpha = 0.1)

model1["var.stats"]

# Exclude SMI

model1 <- ols(dset = EuStockMarkets,
              target = "DAX",
              vars = "CAC FTSE",
              time.series  = TRUE,
              alpha = 0.1)

model1["var.stats"] # both are statistically significant, VIF<10

# Plot of predicted vs. observed values

model1 <- ols(dset = EuStockMarkets,
              target = "DAX",
              vars = "SMI CAC FTSE",
              time.series  = TRUE,
              visualize = TRUE)

# Another way of visualizing time series: obtaining time from "mts" class

date <- as.yearmon(time(EuStockMarkets))
EuStockMarkets <- as.data.frame(EuStockMarkets)
EuStockMarkets$date <- date

model1 <- ols(dset = EuStockMarkets,
              target = "DAX",
              vars = "SMI CAC FTSE",
              time.var  = "date",
              visualize = TRUE)
model1$time.plot
model1$plot

# No intercept

model1 <- ols(dset = EuStockMarkets,
              target = "DAX",
              vars = "SMI CAC FTSE",
              intercept = FALSE)

# Predicted R-Squared

model1 <- ols(dset = EuStockMarkets,
              target = "DAX",
              vars = "SMI CAC FTSE",
              pred.R2 = TRUE)
model1[["stats"]]
model1[["var.stats"]]

# Comparison: pred.R2 turned on and off

mbm.pred.R2 <- microbenchmark(
  "pred.R2"  = {model1 <- ols(dset = EuStockMarkets,
                                        target = "DAX",
                                        vars = "SMI CAC FTSE",
                                        pred.R2 = TRUE)},
  "no.pred.R2" = {model1 <- ols(dset = EuStockMarkets,
                                          target = "DAX",
                                          vars = "SMI CAC FTSE",
                                          pred.R2 = FALSE)})
mbm.pred.R2 <- summary(mbm.pred.R2)
save(mbm.pred.R2, file = "mbm_pred_R2_29012018.RData")
class(mbm.pred.R2)
# load("mbm_pred_R2_29012018.RData")
### Multiple models----

vars <- ncomb(vec = c("SMI", "CAC", "FTSE"),
              m = 1,
              n = 3,
              max.lag = 0)

# Multiple combinations

EuStockMarkets <- data.frame(EuStockMarkets)

vars <- ncomb(vec = c("SMI", "CAC", "FTSE"),
              m = 1,
              n = 3,
              max.lag = 4)
n.models <- length(vars)

EuStockMarkets <- lags(dset = EuStockMarkets,
     vars = c("SMI", "CAC", "FTSE"),
     lag.vec = 1:4)

# Multiple models

models <- ols_summary(dset.sum = EuStockMarkets,
                      target.sum = rep("DAX", n.models),
                      vars.sum = vars,
                      do.parallel = FALSE, intercept.sum = FALSE)
models.stats <- models[[1]]
head(models.stats[order(models.stats$adjusted.R2, decreasing = TRUE),])
head(models.stats[order(models.stats$adjusted.R2, decreasing = TRUE), "model.num"])

models$vars.stats[185]

# Performance

# install.packages("microbenchmark")
library(microbenchmark)

set.seed(2017)

mbm <- microbenchmark(
  "not.parallelized"  = {models <- ols_summary(dset.sum = EuStockMarkets,
                                               target.sum = rep("DAX", n.models),
                                               vars.sum = vars,
                                               do.parallel = FALSE,
                                               progress.bar = FALSE)},
  "parallelized.2.cores" = {models <- ols_summary(dset.sum = EuStockMarkets,
                                          target.sum = rep("DAX", n.models),
                                          vars.sum = vars,
                                          do.parallel = TRUE,
                                          n.cores = 2)},
  "parallelized.3.cores" = {models <- ols_summary(dset.sum = EuStockMarkets,
                                                  target.sum = rep("DAX", n.models),
                                                  vars.sum = vars,
                                                  do.parallel = TRUE,
                                                  n.cores = 3)})
mbm
save(mbm, file = "mbm_16cols_26012018.RData")
# load("mbm_16cols_26012018.RData")
gc()
## Larger number of variables

vars2 <- ncomb(vec = c("SMI", "CAC", "FTSE"),
              m = 1,
              n = 3,
              max.lag = 16)
n.models2 <- length(vars2)

EuStockMarkets <- lags(dset = EuStockMarkets,
                       vars = c("SMI", "CAC", "FTSE"),
                       lag.vec = 1:16)

mbm2 <- microbenchmark(
  "not.parallelized"  = {models <- ols_summary(dset.sum = EuStockMarkets,
                                               target.sum = rep("DAX", n.models2),
                                               vars.sum = vars2,
                                               do.parallel = FALSE,
                                               progress.bar = FALSE)},
  "parallelized.2.cores" = {models <- ols_summary(dset.sum = EuStockMarkets,
                                          target.sum = rep("DAX", n.models2),
                                          vars.sum = vars2,
                                          do.parallel = T,
                                          n.cores = 2)},
  "parallelized.3.cores" = {models <- ols_summary(dset.sum = EuStockMarkets,
                                                  target.sum = rep("DAX", n.models2),
                                                  vars.sum = vars2,
                                                  do.parallel = TRUE,
                                                  n.cores = 3)},
  times = 30)
mbm2
save(mbm2, file = "mbm_52cols_26012018.RData")




# Differences:

EuStockMarkets <- difs(EuStockMarkets, c("DAX", "SMI", "CAC", "FTSE"), 1:4)
vars.dif <- ncomb(vec = c("Diff.SMI.1", "Diff.CAC.1", "Diff.FTSE.1"),
                  m = 1,
                  n = 3,
                  max.lag = 4)
n.models.dif <- length(vars.dif)
models <- ols_summary(dset.sum = EuStockMarkets,
                      target.sum = rep("DAX", n.models.dif),
                      vars.sum = vars.dif,
                      progress.bar = TRUE)


# What if a variables is not in the data set?

models <- ols_summary(dset.sum = EuStockMarkets,
                      target.sum = rep("DAX", n.models),
                      vars.sum = c("Nonexistent.Variable1",
                                   "Nonexistent.Variable2", vars),
                      do.parallel = FALSE,
                      progress.bar = TRUE)
