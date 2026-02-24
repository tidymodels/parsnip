# Execution-time data dimension checks

For some tuning parameters, the range of values depend on the data
dimensions (e.g. `mtry`). Some packages will fail if the parameter
values are outside of these ranges. Since the model might receive
resampled versions of the data, these ranges can't be set prior to the
point where the model is fit. These functions check the possible range
of the data and adjust them if needed (with a warning).

## Usage

``` r
min_cols(num_cols, source)

min_rows(num_rows, source, offset = 0)
```

## Arguments

- num_cols, num_rows:

  The parameter value requested by the user.

- source:

  A data frame for the data to be used in the fit. If the source is
  named "data", it is assumed that one column of the data corresponds to
  an outcome (and is subtracted off).

- offset:

  A number subtracted off of the number of rows available in the data.

## Value

An integer (and perhaps a warning).

## Examples

``` r
nearest_neighbor(neighbors= 100) |>
  set_engine("kknn") |>
  set_mode("regression") |>
  translate()
#> K-Nearest Neighbor Model Specification (regression)
#> 
#> Main Arguments:
#>   neighbors = 100
#> 
#> Computational engine: kknn 
#> 
#> Model fit template:
#> kknn::train.kknn(formula = missing_arg(), data = missing_arg(), 
#>     ks = min_rows(100, data, 5))

library(ranger)
rand_forest(mtry = 2, min_n = 100, trees = 3) |>
  set_engine("ranger") |>
  set_mode("regression") |>
  fit(mpg ~ ., data = mtcars)
#> Warning: ! 100 samples were requested but there were 32 rows in the data.
#> ℹ 32 samples will be used.
#> parsnip model object
#> 
#> Ranger result
#> 
#> Call:
#>  ranger::ranger(x = maybe_data_frame(x), y = y, mtry = min_cols(~2,      x), num.trees = ~3, min.node.size = min_rows(~100, x), num.threads = 1,      verbose = FALSE, seed = sample.int(10^5, 1)) 
#> 
#> Type:                             Regression 
#> Number of trees:                  3 
#> Sample size:                      32 
#> Number of independent variables:  10 
#> Mtry:                             2 
#> Target node size:                 32 
#> Variable importance mode:         none 
#> Splitrule:                        variance 
#> OOB prediction error (MSE):       45.16696 
#> R squared (OOB):                  -0.2434432 
```
