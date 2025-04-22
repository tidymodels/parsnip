


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 3 tuning parameters:

- `mtry`: # Randomly Selected Predictors (type: integer, default: see below)

- `trees`: # Trees (type: integer, default: 500L)

- `min_n`: Minimal Node Size (type: integer, default: see below)

`mtry` depends on the number of columns. The default in [ranger::ranger()] is `floor(sqrt(ncol(x)))`.

`min_n` depends on the mode. For regression, a value of 5 is the default. For classification, a value of 10 is used. 

## Translation from parsnip to the original package (regression)


``` r
rand_forest(
  mtry = integer(1),
  trees = integer(1),
  min_n = integer(1)
) |>  
  set_engine("ranger") |> 
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
## Computational engine: ranger 
## 
## Model fit template:
## ranger::ranger(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     mtry = min_cols(~integer(1), x), num.trees = integer(1), 
##     min.node.size = min_rows(~integer(1), x), num.threads = 1, 
##     verbose = FALSE, seed = sample.int(10^5, 1))
```

`min_rows()` and `min_cols()` will adjust the number of neighbors if the chosen value if it is not consistent with the actual data dimensions.

## Translation from parsnip to the original package (classification)


``` r
rand_forest(
  mtry = integer(1),
  trees = integer(1),
  min_n = integer(1)
) |> 
  set_engine("ranger") |> 
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
## Computational engine: ranger 
## 
## Model fit template:
## ranger::ranger(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     mtry = min_cols(~integer(1), x), num.trees = integer(1), 
##     min.node.size = min_rows(~integer(1), x), num.threads = 1, 
##     verbose = FALSE, seed = sample.int(10^5, 1), probability = TRUE)
```

Note that a `ranger` probability forest is always fit (unless the `probability` argument is changed by the user via [set_engine()]).

## Preprocessing requirements


This engine does not require any special encoding of the predictors. Categorical predictors can be partitioned into groups of factor levels (e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables are not required for this model. 

## Other notes

By default, parallel processing is turned off. When tuning, it is more efficient to parallelize over the resamples and tuning parameters. To parallelize the construction of the trees within the `ranger` model, change the `num.threads` argument via [set_engine()]. 

For `ranger` confidence intervals, the intervals are  constructed using the form `estimate +/- z * std_error`. For  classification probabilities, these values can fall outside of  `[0, 1]` and will be coerced to be in this range.

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

## Sparse Data


This model can utilize sparse data during model fitting and prediction. Both sparse matrices such as dgCMatrix from the `Matrix` package and sparse tibbles from the `sparsevctrs` package are supported. See [sparse_data] for more information.

While this engine supports sparse data as an input, it doesn't use it any differently than dense data. Hence there it no reason to convert back and forth.

## Saving fitted model objects


This model object contains data that are not required to make predictions. When saving the model for the purpose of prediction, the size of the saved object might be substantially reduced by using functions from the [butcher](https://butcher.tidymodels.org) package.


## Examples 

The "Fitting and Predicting with parsnip" article contains [examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#rand-forest-ranger) for `rand_forest()` with the `"ranger"` engine.

## References

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.
