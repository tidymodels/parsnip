


For this engine, there are multiple modes: classification and regression. However, multiclass classification is not supported yet.

## Tuning Parameters



This model has 7 tuning parameters:

- `tree_depth`: Tree Depth (type: integer, default: 5L)

- `trees`: # Trees (type: integer, default: 20L)

- `learn_rate`: Learning Rate (type: double, default: 0.1)

- `mtry`: # Randomly Selected Predictors (type: integer, default: see below)

- `min_n`: Minimal Node Size (type: integer, default: 1L)

- `loss_reduction`: Minimum Loss Reduction (type: double, default: 0.0)

- `sample_size`: # Observations Sampled (type: integer, default: 1.0)

The `mtry` parameter is related to the number of predictors. The default depends on the model mode. For classification, the square root of the number of predictors is used and for regression, one third of the predictors are sampled. 

## Translation from parsnip to the original package (regression)


``` r
boost_tree(
  mtry = integer(), trees = integer(), min_n = integer(), tree_depth = integer(),
  learn_rate = numeric(), loss_reduction = numeric(), sample_size = numeric()
) |>
  set_engine("spark") |>
  set_mode("regression") |>
  translate()
```

```
## Boosted Tree Model Specification (regression)
## 
## Main Arguments:
##   mtry = integer()
##   trees = integer()
##   min_n = integer()
##   tree_depth = integer()
##   learn_rate = numeric()
##   loss_reduction = numeric()
##   sample_size = numeric()
## 
## Computational engine: spark 
## 
## Model fit template:
## sparklyr::ml_gradient_boosted_trees(x = missing_arg(), formula = missing_arg(), 
##     type = "regression", feature_subset_strategy = integer(), 
##     max_iter = integer(), min_instances_per_node = min_rows(integer(0), 
##         x), max_depth = integer(), step_size = numeric(), min_info_gain = numeric(), 
##     subsampling_rate = numeric(), seed = sample.int(10^5, 1))
```

## Translation from parsnip to the original package (classification)


``` r
boost_tree(
  mtry = integer(), trees = integer(), min_n = integer(), tree_depth = integer(),
  learn_rate = numeric(), loss_reduction = numeric(), sample_size = numeric()
) |> 
  set_engine("spark") |> 
  set_mode("classification") |> 
  translate()
```

```
## Boosted Tree Model Specification (classification)
## 
## Main Arguments:
##   mtry = integer()
##   trees = integer()
##   min_n = integer()
##   tree_depth = integer()
##   learn_rate = numeric()
##   loss_reduction = numeric()
##   sample_size = numeric()
## 
## Computational engine: spark 
## 
## Model fit template:
## sparklyr::ml_gradient_boosted_trees(x = missing_arg(), formula = missing_arg(), 
##     type = "classification", feature_subset_strategy = integer(), 
##     max_iter = integer(), min_instances_per_node = min_rows(integer(0), 
##         x), max_depth = integer(), step_size = numeric(), min_info_gain = numeric(), 
##     subsampling_rate = numeric(), seed = sample.int(10^5, 1))
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

 - Luraschi, J, K Kuo, and E Ruiz. 2019. _Mastering Spark with R_. O'Reilly Media

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.

