22 JAN 2018

README
======

The goal of functions in this repo is to semi automatically build Ordinary Least Squares (OLS) models on the basis of a set of declared variables. If the available data are time series, lagging and differencing is possible.

 

1. Short description of the functions:
--------------------------------------

1.  `ols`: Function allows building one OLS model and performs statistical tests.
2.  `ols_summary`: Function allows building multiple OLS models and lets the user parallelization of computations (if required).
3.  `ncomb`: Allows all possible combinations of a set of variables (including the ones after differencing).
4.  `lags`: Allows lagging variables.
5.  `difs`: Allows differencing variables.

 

2. Tutorial
-----------

This tutorial will teach you how to use above mentioned functions. After getting through below given examples, you'll be able to:

1.  Build one regression model and test it.
2.  Visualize histogram of errors.
3.  Difference and lag selected variables.
4.  Build multiple regression models and test them.
5.  Use parallelization while building multiple models.
6.  Decide if utilizing parallelization actually shortens computation time.

### 2.0 Required packages

Before starting our tutorial, a few packages should be installed *a priori*:

``` r
install.packages("caret")
install.packages("lmtest")
install.packages("car")
install.packages("nortest")
install.packages("scales")
install.packages("strucchange")
install.packages("lubridate")
install.packages("nortest")
install.packages("microbenchmark")
```

### 2.1 Our first regression model

Firstly, let's read the data. We will take advantage of `EuStockMarkets` from library `datasets`:

``` r
library(datasets)
data(EuStockMarkets)
```

Let's take a look at a couple of rows:

``` r
head(EuStockMarkets)
```

|      DAX|     SMI|     CAC|    FTSE|
|--------:|-------:|-------:|-------:|
|  1628.75|  1678.1|  1772.8|  2443.6|
|  1613.63|  1688.5|  1750.5|  2460.2|
|  1606.51|  1678.6|  1718.0|  2448.2|
|  1621.04|  1684.1|  1708.1|  2470.4|
|  1618.16|  1686.6|  1723.1|  2484.7|
|  1610.61|  1671.6|  1714.3|  2466.8|

The data set consists of 4 time series - stock indexes. In order to build our first OLS models, we can use the function `ols`:

``` r
model1 <- ols(dset = EuStockMarkets,
              target = "DAX",
              vars = "SMI CAC FTSE")
```

The first argument, `dset`, is the data set we want to use. `target` is our target variable; `vars` is a string containing variable names separated by single blanks (spaces). Let’s take a look at names of consecutive elements of `model1`:

``` r
names(model1)
```

    ## [1] "stats"            "var.stats"        "plot"            
    ## [4] "output.residuals" "time.plot"

We can see that the object consists of 5 elements. We will start our analysis with the first one, `stats`:

``` r
model1$stats
```

| target | vars         |         R2|  adjusted.R2|      RMSE|    F.stat|  F.p.value|  bp.stat|  bp.p.value|   bg.stat|  bg.p.value|  reset.stat|  reset.p.value|   ad.stat|  ad.p.value|    sw.stat|  sw.p.value|  chow.stat|  chow.p.value|   max.vif| tests |     n| equation                                                                                                  |
|:-------|:-------------|----------:|------------:|---------:|---------:|----------:|--------:|-----------:|---------:|-----------:|-----------:|--------------:|---------:|-----------:|----------:|-----------:|----------:|-------------:|---------:|:------|-----:|:----------------------------------------------------------------------------------------------------------|
| DAX    | SMI CAC FTSE |  0.9898478|    0.9898314|  803.3033|  60320.47|          0|  243.397|           0|  1803.221|           0|    47.01741|              0|  10.44616|           0|  0.9862875|           0|   144.7465|             0|  100.8572| TRUE  |  1860| (Intercept)*(-175.945668313583)+SMI*(0.49277225460085)+CAC*(0.495653787472762)+FTSE*(-0.0172026329222922) |

The output presents a flat table, showing the target variable, the independent variables, statistics representing the model quality (R-Squared, adjusted R-Squared, RMSE) and some statistical tests:

1.  **F**: significance test of all variables in the model.
2.  **bp**: Breusch-Pagan heteroscedasticity test.
3.  **bg**: Breusch-Godfrey autocorrelation test.
4.  **reset**: RESET specification test (informs whether the linear model can be applied for such target and independent variables).
5.  **ad**: Anderson-Darling normality of error term test.
6.  **sw**: Shapiro-Wilk normality of error term test (for small samples, n&lt;20).
7.  **chow**: Chow test checking stability of coefficients in time.
8.  **max.vif**: Maximum VIF (Variance Inflation Factor) in the model. Additionally, the *tests* column informs whether the model complies with tests from points 2-7. *n* is the number of observations, on which the model was built. The last column shows the equation of a given regression.

The table `var.stats` show statistics of regressors:

``` r
model1$var.stats
```

| var         |          coef|    p.value|        vif|
|:------------|-------------:|----------:|----------:|
| (Intercept) |  -175.9456683|  0.0000848|         NA|
| SMI         |     0.4927723|  0.0000000|  100.85721|
| CAC         |     0.4956538|  0.0000000|   12.47012|
| FTSE        |    -0.0172026|  0.4103735|   64.68514|

The consecutive columns shows variable names, their coefficients, p.values and VIFs. We can see that VIFs of `SMI` and `FTSE` are way beyond 10 (one of many literature threshold for this indicator). We will cope with this problem later.

The default p-value for the regression is `alpha = 0.05`. This parameter can also be changed:

``` r
model1 <- ols(dset = EuStockMarkets,
              target = "DAX",
              vars = "SMI CAC FTSE",
              alpha = 0.1)
model1["var.stats"]
```

    ## $var.stats
    ##                     var          coef       p.value       vif
    ## (Intercept) (Intercept) -175.94566831  8.479065e-05        NA
    ## SMI                 SMI    0.49277225 9.020895e-181 100.85721
    ## CAC                 CAC    0.49565379 2.999041e-180  12.47012
    ## FTSE               FTSE   -0.01720263  4.103735e-01  64.68514

As mentioned above, multicollinearity was observed. In order to get rid of it, we can exclude the `SMI` index from the model:

``` r
model2 <- ols(dset = EuStockMarkets,
                target = "DAX",
                vars = "CAC FTSE")
model2$var.stats
```

| var         |           coef|  p.value|       vif|
|:------------|--------------:|--------:|---------:|
| (Intercept) |  -1575.3045205|        0|        NA|
| CAC         |      0.8479211|        0|  6.194059|
| FTSE        |      0.6217500|        0|  6.194059|

### 2.2 Visualizations

We can output residuals of the regression by using the argument `output.residuals`:

``` r
model1 <- ols(dset = EuStockMarkets,
                target = "DAX",
                vars = "SMI CAC FTSE",
                visualize = T,
                output.residuals = T)
qplot(model1[["output.residuals"]]) +
  theme_minimal() +
  xlab('') +
  ggtitle("Histogram of residuals") # plot
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-markdown_github/residuals-1.png)

The `EuStockMarkets` data set is an object of `mts` class. That means that it includes dates. We can take advantage of it and visualize predicted and observed values of `DAX` on one plot dependent on time. To do it,`time.series` and `visualize` arguments are used:

``` r
model1 <- ols(dset = EuStockMarkets,
              target = "DAX",
              vars = "CAC FTSE",
              time.series  = T,
              visualize = T)
model1$time.plot
```

    ## Don't know how to automatically pick scale for object of type yearmon. Defaulting to continuous.

![](README_files/figure-markdown_github/predicted_vs_observed_plot-1.png)

### 2.3 Differencing and lagging variables

Now we will proceed to building multiple models. At the moment objects of class `mts` are not handled, so at first our data set should be converted into a data frame. Then we can add some lagged variables. To lag regressors, we can use `lag` function:

``` r
EuStockMarkets <- data.frame(EuStockMarkets)

EuStockMarkets <- lags(dset = EuStockMarkets,
     vars = c("SMI", "CAC", "FTSE"),
     lag.vec = 1:4)
```

`dset` is the data set, in which variables can be found. The argument`vars` selects variables, which should be lagged. `lag.vec` is a vector of lags which should be executed. In this case, there are four lags: the minimum lag is `1`, and the maximum is `4`. Lagged variables have a suffix `.<lag.order.`, e.g. the `SME` index lagged by with a lag of three periods is called `SMI.3`.

Having lagged variables, we can proceed to obtaining vectors of all the possible combinations of lagged (and unlagged) regressors. To do this, we will use the function `ncomb`:

``` r
vars <- ncomb(vec = c("SMI", "CAC", "FTSE"),
              m = 1,
              n = 3,
              max.lag = 4)
```

The vector `vec` indicates, which variables should be taken into consideration when making combinations. `m` is a minimum number in combination and `n` is maximum number of variables in combination. `max.lag` indicates maximum lag of variables, which should be used when making combinations.

Let's see the result of `ncomb`:

``` r
head(vars, 10)
```

    ##  [1] "SMI.1" "SMI.2" "SMI.3" "SMI.4" "SMI"   "CAC.1" "CAC.2" "CAC.3"
    ##  [9] "CAC.4" "CAC"

``` r
tail(vars, 10)
```

    ##  [1] "SMI.1 CAC.4 FTSE" "SMI.2 CAC.4 FTSE" "SMI.3 CAC.4 FTSE"
    ##  [4] "SMI.4 CAC.4 FTSE" "SMI CAC.4 FTSE"   "SMI.1 CAC FTSE"  
    ##  [7] "SMI.2 CAC FTSE"   "SMI.3 CAC FTSE"   "SMI.4 CAC FTSE"  
    ## [10] "SMI CAC FTSE"

and the number of produced combinations

``` r
n.models <- length(vars)
n.models
```

    ## [1] 215

### 2.4 Creating multiple regression models

As we can see, in a moment we will build 215 OLS models. To obtain them in one command, `ols_summary` function will be used:

``` r
models <- ols_summary(dset.sum = EuStockMarkets,
                      target.sum = rep("DAX", n.models),
                      vars.sum = vars)
```

`dset.sum` is our converted data set, `target.sum` is target variable repeated `n.models` times (since we want to build such a number of regressions) and `vars.sum` is a vector of all combinations of regressors produced by `ncomb` function.

The results are of the same structure as in case of our first model (obtained from `ols` function). The only additional column we get is `model.num`, which is a model number:

``` r
models.stats <- models$stats
models.stats$model.num
```

    ##   [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17
    ##  [18]  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34
    ##  [35]  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51
    ##  [52]  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68
    ##  [69]  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85
    ##  [86]  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102
    ## [103] 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119
    ## [120] 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136
    ## [137] 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153
    ## [154] 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170
    ## [171] 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187
    ## [188] 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204
    ## [205] 205 206 207 208 209 210 211 212 213 214 215

Let's see six best models in terms of adjusted R-Squared:

``` r
head(models.stats[order(models.stats$adjusted.R2, decreasing = T),])
```

|     | target | vars             |         R2|  adjusted.R2|      RMSE|    F.stat|  F.p.value|   bp.stat|  bp.p.value|   bg.stat|  bg.p.value|  reset.stat|  reset.p.value|    ad.stat|  ad.p.value|    sw.stat|  sw.p.value|  chow.stat|  chow.p.value|     max.vif| tests |     n| equation                                                                                                       |  model.num|
|-----|:-------|:-----------------|----------:|------------:|---------:|---------:|----------:|---------:|-----------:|---------:|-----------:|-----------:|--------------:|----------:|-----------:|----------:|-----------:|----------:|-------------:|-----------:|:------|-----:|:---------------------------------------------------------------------------------------------------------------|----------:|
| 185 | DAX    | SMI CAC.4 FTSE.4 |  0.9899472|    0.9899309|  803.4522|  60791.90|          0|  204.6968|           0|  1761.647|           0|    45.56233|              0|  10.304760|           0|  0.9867456|           0|   134.6473|             0|   85.364357| TRUE  |  1856| (Intercept)*(-127.626973618169)+SMI*(0.513114108395759)+CAC.4*(0.482952957049387)+FTSE.4*(-0.0416017513798577) |        185|
| 160 | DAX    | SMI CAC.4 FTSE.3 |  0.9899393|    0.9899230|  803.9688|  60743.76|          0|  206.9403|           0|  1758.698|           0|    47.30768|              0|  10.094337|           0|  0.9869969|           0|   133.6170|             0|   89.611317| TRUE  |  1856| (Intercept)*(-140.068438037302)+SMI*(0.50873290702042)+CAC.4*(0.484290355238345)+FTSE.3*(-0.034770345076648)   |        160|
| 135 | DAX    | SMI CAC.4 FTSE.2 |  0.9899310|    0.9899147|  804.4874|  60693.01|          0|  209.3146|           0|  1758.335|           0|    49.77853|              0|   9.865628|           0|  0.9872799|           0|   133.0109|             0|   94.180270| TRUE  |  1856| (Intercept)*(-158.998174628224)+SMI*(0.502095876874254)+CAC.4*(0.486880719902912)+FTSE.2*(-0.0247679406452226) |        135|
| 35  | DAX    | SMI CAC.4        |  0.9899230|    0.9899121|  700.2205|  91015.77|          0|  211.9794|           0|  1755.853|           0|    45.51831|              0|   9.365358|           0|  0.9878405|           0|   170.6485|             0|    9.181714| TRUE  |  1856| (Intercept)*(-207.684673377669)+SMI*(0.485097308509324)+CAC.4\*(0.494904389563161)                             |         35|
| 110 | DAX    | SMI CAC.4 FTSE.1 |  0.9899247|    0.9899083|  804.9980|  60654.42|          0|  211.9374|           0|  1757.695|           0|    53.06066|              0|   9.579326|           0|  0.9876018|           0|   133.3751|             0|   98.990796| TRUE  |  1856| (Intercept)*(-184.955738670181)+SMI*(0.493022354676854)+CAC.4*(0.490983988133413)+FTSE.1*(-0.0114386271974997) |        110|
| 210 | DAX    | SMI CAC.4 FTSE   |  0.9899234|    0.9899070|  805.5190|  60646.48|          0|  214.9618|           0|  1755.931|           0|    57.17214|              0|   9.268893|           0|  0.9879479|           0|   134.8465|             0|  103.232524| TRUE  |  1856| (Intercept)*(-218.221847587644)+SMI*(0.481427677619494)+CAC.4*(0.496796260977783)+FTSE*(0.00525002049348799)   |        210|

As it can be noticed, the best model was the 185th one. We can see it by looking it up in `vars.stats` list:

``` r
vars.stats <- models$vars.stats
vars.stats[[185]]
```

|             | var         |          coef|    p.value|       vif|
|-------------|:------------|-------------:|----------:|---------:|
| (Intercept) | (Intercept) |  -127.6269736|  0.0019513|        NA|
| SMI         | SMI         |     0.5131141|  0.0000000|  85.36436|
| CAC.4       | CAC.4       |     0.4829530|  0.0000000|  10.84197|
| FTSE.4      | FTSE.4      |    -0.0416018|  0.0348734|  57.61019|

What happens if a variable which is not in the data set is declared in `ols_summary`? Let's check it out:

``` r
models <- ols_summary(dset.sum = EuStockMarkets,
                      target.sum = rep("DAX", n.models),
                      vars.sum = c("Nonexistent.Variable1",
                                   "Nonexistent.Variable2", vars))
```

    ## Error in ols_summary(dset.sum = EuStockMarkets, target.sum = rep("DAX", : Variable(s):
    ##  Nonexistent.Variable1, Nonexistent.Variable2
    ##  is/are not in the data set.

### 2.5 Parallelization

We can parallelize our computations by using `do.parallel` argument. By default, the number of used cores is the `maximum.number.of.cores - 2`. This variable can be controlled by `n.cores`:

``` r
models <- ols_summary(dset.sum = EuStockMarkets,
                      target.sum = rep("DAX", n.models),
                      vars.sum = vars,
                      do.parallel = T,
                      n.cores = 2)
```

Parallelization is turned off by default.

### 2.6 Benchmark of parallelized and non-parallelized computations

Do parallelized calculations actually execute faster than non-parallelized ones? In order to check it, the library `microbenchmark` was used, i.e. each function was ran 100 times:

``` r
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
```

``` r
mbm
```

The table below presents statistic (mean, median etc.) of time required to execute a function (in seconds):

| expr             |        min|         lq|       mean|     median|         uq|       max|  neval|
|:-----------------|----------:|----------:|----------:|----------:|----------:|---------:|------:|
| not.parallelized |   9.029895|   9.276693|   9.632542|   9.420636|   9.742205|  12.48951|    100|
| parallelized     |  10.462151|  10.691725|  11.432108|  11.103139|  11.789745|  20.04804|    100|

As it can be seen, the parallelization did not reduce calculation time. Such a result comes from time required to initialize a cluster, which takes a couple of seconds. If calculations of ~200 regressions last a few seconds, employing additional cores doesn’t make sense.

Will such a phenomenon hold for larger data? Let's find out by adding lags up to 16 and running the benchmark again (this time only 10 times due to the computations cost). In each iteration of each function, 5831 models will be build:

``` r
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
```

``` r
mbm2
```

| expr             |       min|        lq|      mean|    median|        uq|       max|  neval|
|:-----------------|---------:|---------:|---------:|---------:|---------:|---------:|------:|
| not.parallelized |  260.8858|  261.0712|  268.7942|  268.1902|  275.5850|  278.8869|     10|
| parallelized     |  160.4067|  162.7916|  164.4797|  163.9955|  165.0944|  170.4291|     10|

In this case, parallelization shortened the time required to run a function call. It should be taken into consideration that the number of models was 27 times greater than previously.

As a rule of thumb, when dealing with a couple hundred of models, the computations should not be parallelized. When the number of models is at least a few thousands, additional cores can help.
