


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 2 tuning parameters:

- `tree_depth`: Tree Depth (type: integer, default: 5L)

- `min_n`: Minimal Node Size (type: integer, default: 1L)

## Translation from parsnip to the original package (classification)


``` r
decision_tree(tree_depth = integer(1), min_n = integer(1)) |> 
  set_engine("spark") |> 
  set_mode("classification") |> 
  translate()
```

```
## Decision Tree Model Specification (classification)
## 
## Main Arguments:
##   tree_depth = integer(1)
##   min_n = integer(1)
## 
## Computational engine: spark 
## 
## Model fit template:
## sparklyr::ml_decision_tree_classifier(x = missing_arg(), formula = missing_arg(), 
##     max_depth = integer(1), min_instances_per_node = min_rows(0L, 
##         x), seed = sample.int(10^5, 1))
```


## Translation from parsnip to the original package (regression)


``` r
decision_tree(tree_depth = integer(1), min_n = integer(1)) |> 
  set_engine("spark") |> 
  set_mode("regression") |> 
  translate()
```

```
## Decision Tree Model Specification (regression)
## 
## Main Arguments:
##   tree_depth = integer(1)
##   min_n = integer(1)
## 
## Computational engine: spark 
## 
## Model fit template:
## sparklyr::ml_decision_tree_regressor(x = missing_arg(), formula = missing_arg(), 
##     max_depth = integer(1), min_instances_per_node = min_rows(0L, 
##         x), seed = sample.int(10^5, 1))
```

## Preprocessing requirements


This engine does not require any special encoding of the predictors. Categorical predictors can be partitioned into groups of factor levels (e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables are not required for this model. 

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

Note that, for spark engines, the `case_weight` argument value should be a character string to specify the column with the numeric case weights. 

## Other details


For models created using the `"spark"` engine, there are several things to consider. 

* Only the formula interface to via `fit()` is available; using `fit_xy()` will generate an error. 
* The predictions will always be in a Spark table format. The names will be the same as documented but without the dots. 
* There is no equivalent to factor columns in Spark tables so class predictions are returned as character columns. 
* To retain the model object for a new R session (via `save()`), the `model$fit` element of the parsnip object should be serialized via `ml_save(object$fit)` and separately saved to disk. In a new session, the object can be reloaded and reattached to the parsnip object.

## References

-   Kuhn, M, and K Johnson. 2013. *Applied Predictive Modeling*. Springer.

