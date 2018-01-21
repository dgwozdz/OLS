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

setwd('F:/Damian/github/ols/OLS')

library(datasets)
library(utils)
data(EuStockMarkets)

functions <- c("ols.R", "ols_summary.R", "ncomb.R",
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
                visualize = T,
                output.residuals = T)
model1[["output.residuals"]] # plot

# Change p-value

model1 <- ols(dset = EuStockMarkets,
              target = "DAX",
              vars = "SMI CAC FTSE",
              time.series  = T,
              alpha = 0.1)

model1["var.stats"]

# Exclude SMI

model1 <- ols(dset = EuStockMarkets,
              target = "DAX",
              vars = "CAC FTSE",
              time.series  = T,
              alpha = 0.1)

model1["var.stats"] # both are statistically significant, VIF<10

# Plot of predicted vs. observed values

model1 <- ols(dset = EuStockMarkets,
              target = "DAX",
              vars = "SMI CAC FTSE",
              time.series  = T)
class(EuStockMarkets)

### Multiple models----

vars <- ncomb(vec = c("SMI", "CAC", "FTSE"),
              m = 1,
              n = 3,
              max.lag = 0)
vars

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
                      do.parallel = T)
models.stats <- models[[1]]
head(models.stats[order(models.stats$adjusted.R2, decreasing = T),])
head(models.stats[order(models.stats$adjusted.R2, decreasing = T), "model.num"])

models$vars.stats[185]

# Performance

# install.packages("microbenchmark")
library(microbenchmark)

set.seed(2017)

mbm <- microbenchmark(
  "not.parallelized"  = {models <- ols_summary(dset.sum = EuStockMarkets,
                                               target.sum = rep("DAX", n.models),
                                               vars.sum = vars,
                                               do.parallel = F,
                                               progress.bar = F)},
  "parallelized" = {models <- ols_summary(dset.sum = EuStockMarkets,
                                          target.sum = rep("DAX", n.models),
                                          vars.sum = vars,
                                          do.parallel = T)})
mbm
save(mbm, file = "mbm_16cols_21012018.RData")


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
                                               do.parallel = F,
                                               progress.bar = F)},
  "parallelized" = {models <- ols_summary(dset.sum = EuStockMarkets,
                                          target.sum = rep("DAX", n.models2),
                                          vars.sum = vars2,
                                          do.parallel = T)},
  times = 10)
mbm2
save(mbm2, file = "mbm_52cols_21012018.RData")




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
                      progress.bar = T)


# What if a variables is not in the data set?

models <- ols_summary(dset.sum = EuStockMarkets,
                      target.sum = rep("DAX", n.models),
                      vars.sum = c("Nonexistent.Variable1",
                                   "Nonexistent.Variable2", vars),
                      do.parallel = F,
                      progress.bar = T)
