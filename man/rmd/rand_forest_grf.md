


For this engine, there are multiple modes: classification, regression, and quantile regression

## Tuning Parameters



This model has 3 tuning parameters:

- `mtry`: # Randomly Selected Predictors (type: integer, default: see below)

- `trees`: # Trees (type: integer, default: 2000L)

- `min_n`: Minimal Node Size (type: integer, default: 5L)

`mtry` depends on the number of columns. If there are `p` predictors, the default value of `mtry` is `min(ceiling(sqrt(p) + 20), p)`. 

## Translation from parsnip to the original package (regression)

See [`?regression_forest`](https://grf-labs.github.io/grf/reference/regression_forest.html)


``` r
rand_forest(
  mtry = integer(1),
  trees = integer(1),
  min_n = integer(1)
) |>  
  set_engine("grf") |> 
  set_mode("regression") |> 
  translate()
```

```
## Random Forest Model Specification (regression)
## 
## Main Arguments:
##   mtry = integer(1)
##   trees = integer(1)
##   min_n = integer(1)
## 
## Computational engine: grf 
## 
## Model fit template:
## grf::regression_forest(X = missing_arg(), Y = missing_arg(), 
##     weights = missing_arg(), mtry = min_cols(~integer(1), x), 
##     num.trees = integer(1), min.node.size = min_rows(~integer(1), 
##         x), num.threads = 1)
```

## Translation from parsnip to the original package (classification)

See [`?probability_forest`](https://grf-labs.github.io/grf/reference/probability_forest.html)


``` r
rand_forest(
  mtry = integer(1),
  trees = integer(1),
  min_n = integer(1)
) |> 
  set_engine("grf") |> 
  set_mode("classification") |> 
  translate()
```

```
## Random Forest Model Specification (classification)
## 
## Main Arguments:
##   mtry = integer(1)
##   trees = integer(1)
##   min_n = integer(1)
## 
## Computational engine: grf 
## 
## Model fit template:
## grf::probability_forest(X = missing_arg(), Y = missing_arg(), 
##     weights = missing_arg(), mtry = min_cols(~integer(1), x), 
##     num.trees = integer(1), min.node.size = min_rows(~integer(1), 
##         x), num.threads = 1)
```

## Translation from parsnip to the original package (quantile regression)

See [`?quantile_forest`](https://grf-labs.github.io/grf/reference/quantile_forest.html)

When specifying _any_ quantile regression model, the user must specify the quantile levels _a priori_. 


``` r
rand_forest(
  mtry = integer(1),
  trees = integer(1),
  min_n = integer(1)
) |> 
  set_engine("grf") |> 
  set_mode("quantile regression", quantile_levels = (1:3) / 4) |> 
  translate()
```

```
## Random Forest Model Specification (quantile regression)
## 
## Main Arguments:
##   mtry = integer(1)
##   trees = integer(1)
##   min_n = integer(1)
## 
## Computational engine: grf 
## 
## Model fit template:
## grf::quantile_forest(X = missing_arg(), Y = missing_arg(), mtry = min_cols(~integer(1), 
##     x), num.trees = integer(1), min.node.size = min_rows(~integer(1), 
##     x), num.threads = 1, quantiles = quantile_levels)
```

```
## Quantile levels: 0.25, 0.5, and 0.75.
```

## Preprocessing requirements

This method _does_ require qualitative predictors to be converted to a numeric format (manually). When using parsnip, a one-hot encoding is automatically used to do this. 

If there are missing values in the predictors, the model will use case-wise deletion to remove them. 

## Other notes

By default, parallel processing is turned off. When tuning, it is more efficient to parallelize over the resamples and tuning parameters. To parallelize the construction of the trees within the `grf` model, change the `num.threads` argument via [set_engine()]. 

For `grf` confidence intervals, the intervals are constructed using the form `estimate +/- z * std_error`. For classification probabilities, these values can fall outside of  `[0, 1]` and will be coerced to be in this range.

## Case weights

The regression and classification models enable the use of case weights. The quantile regression mode does not. 

## Examples 

The "Fitting and Predicting with parsnip" [article](https://www.tidymodels.org/learn/models/parsnip-predictions/) contains examples for `rand_forest()` with the `"grf"` engine.

## References

Athey, Susan, Julie Tibshirani, and Stefan Wager. "Generalized Random Forests". _Annals of Statistics_, 47(2), 2019.

