# OLS
A set of functions to build multiple Ordinary Least Squares models in a parallelized way and visualize them.

Main function: ols_summary (builds multiple OLS models in a parallelized way).

### PARAMETERS:

1) dset.sum - input data set
2) target.sum - vector of target variables declared as strings
3) vars.sum - vector of independent variables declared as strings
              and separated by blanks in each string
4) alpha.sum - significance level
5) intercept.sum - a boolean value indicating whether the built model
              should have an intercept
6) do.parallel - a boolean value indicating whether parallelization
              should be used
7) n.cores - number of utilized cores (active only if do.parallel == T)
6) visualize.sum  - a boolean value indicating whether the built model
              should be visualized [CURRENTLY INACTIVE]
              
### EXAMPLE:

-
