


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 3 tuning parameters:

- `mtry`: # Randomly Selected Predictors (type: integer, default: see below)

- `trees`: # Trees (type: integer, default: 20L)

- `min_n`: Minimal Node Size (type: integer, default: 1L)

`mtry` depends on the number of columns and the model mode. The default in [sparklyr::ml_random_forest()] is `floor(sqrt(ncol(x)))` for classification and `floor(ncol(x)/3)` for regression.

## Translation from parsnip to the original package (regression)


``` r
rand_forest(
  mtry = integer(1),
  trees = integer(1),
  min_n = integer(1)
) |>  
  set_engine("spark") |> 
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
## Computational engine: spark 
## 
## Model fit template:
## sparklyr::ml_random_forest(x = missing_arg(), formula = missing_arg(), 
##     type = "regression", feature_subset_strategy = integer(1), 
##     num_trees = integer(1), min_instances_per_node = min_rows(~integer(1), 
##         x), seed = sample.int(10^5, 1))
```

`min_rows()` and `min_cols()` will adjust the number of neighbors if the chosen value if it is not consistent with the actual data dimensions.

## Translation from parsnip to the original package (classification)


``` r
rand_forest(
  mtry = integer(1),
  trees = integer(1),
  min_n = integer(1)
) |> 
  set_engine("spark") |> 
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
## Computational engine: spark 
## 
## Model fit template:
## sparklyr::ml_random_forest(x = missing_arg(), formula = missing_arg(), 
##     type = "classification", feature_subset_strategy = integer(1), 
##     num_trees = integer(1), min_instances_per_node = min_rows(~integer(1), 
##         x), seed = sample.int(10^5, 1))
```

## Preprocessing requirements


This engine does not require any special encoding of the predictors. Categorical predictors can be partitioned into groups of factor levels (e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables are not required for this model. 

## Other details


For models created using the `"spark"` engine, there are several things to consider. 

* Only the formula interface to via `fit()` is available; using `fit_xy()` will generate an error. 
* The predictions will always be in a Spark table format. The names will be the same as documented but without the dots. 
* There is no equivalent to factor columns in Spark tables so class predictions are returned as character columns. 
* To retain the model object for a new R session (via `save()`), the `model$fit` element of the parsnip object should be serialized via `ml_save(object$fit)` and separately saved to disk. In a new session, the object can be reloaded and reattached to the parsnip object.

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

Note that, for spark engines, the `case_weight` argument value should be a character string to specify the column with the numeric case weights. 


## References

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.

