


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 8 tuning parameters:

- `trees`: # Trees (type: integer, default: 50)

- `tree_depth`: Tree Depth (type: integer, default: 6)

- `min_n`: Minimal Node Size (type: integer, default: 1)

- `learn_rate`: Learning Rate (type: double, default: 0.3)

- `sample_size`: # Observations Sampled (type: integer, default: 1)

- `mtry`: # Randomly Selected Predictors (type: integer, default: 1)

- `loss_reduction`: Minimum Loss Reduction (type: double, default: 0)

- `stop_iter`: # Iterations Before Stopping (type: integer, default: 0)

`min_n` represents the fewest allowed observations in a terminal node, [h2o::h2o.xgboost()] allows only one row in a leaf by default. 

`stop_iter` controls early stopping rounds based on the convergence of the engine parameter `stopping_metric`. By default, [h2o::h2o.xgboost()] does not use early stopping. When `stop_iter` is not 0, [h2o::h2o.xgboost()] uses logloss for classification, deviance for regression and anonomaly score for Isolation Forest. This is mostly useful when used alongside the engine parameter `validation`, which is the __proportion__ of train-validation split, parsnip will split and pass the two data frames to h2o. Then [h2o::h2o.xgboost()] will evaluate the metric and early stopping criteria on the validation set. 

## Translation from parsnip to the original package (regression)

[agua::h2o_train_xgboost()] is a wrapper around [h2o::h2o.xgboost()]. 

The **agua** extension package is required to fit this model.


``` r
boost_tree(
  mtry = integer(), trees = integer(), tree_depth = integer(), 
  learn_rate = numeric(), min_n = integer(), loss_reduction = numeric(), stop_iter = integer()
) |>
  set_engine("h2o") |>
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
##   stop_iter = integer()
## 
## Computational engine: h2o 
## 
## Model fit template:
## agua::h2o_train_xgboost(x = missing_arg(), y = missing_arg(), 
##     weights = missing_arg(), validation_frame = missing_arg(), 
##     col_sample_rate = integer(), ntrees = integer(), min_rows = integer(), 
##     max_depth = integer(), learn_rate = numeric(), min_split_improvement = numeric(), 
##     stopping_rounds = integer())
```

## Translation from parsnip to the original package (classification)

The **agua** extension package is required to fit this model.


``` r
boost_tree(
  mtry = integer(), trees = integer(), tree_depth = integer(), 
  learn_rate = numeric(), min_n = integer(), loss_reduction = numeric(), stop_iter = integer()
) |> 
  set_engine("h2o") |> 
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
##   stop_iter = integer()
## 
## Computational engine: h2o 
## 
## Model fit template:
## agua::h2o_train_xgboost(x = missing_arg(), y = missing_arg(), 
##     weights = missing_arg(), validation_frame = missing_arg(), 
##     col_sample_rate = integer(), ntrees = integer(), min_rows = integer(), 
##     max_depth = integer(), learn_rate = numeric(), min_split_improvement = numeric(), 
##     stopping_rounds = integer())
```


## Preprocessing


This engine does not require any special encoding of the predictors. Categorical predictors can be partitioned into groups of factor levels (e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables are not required for this model. 

Non-numeric predictors (i.e., factors) are internally converted to numeric. In the classification context, non-numeric outcomes (i.e., factors) are also internally converted to numeric. 

## Interpreting `mtry`


The `mtry` argument denotes the number of predictors that will be randomly sampled at each split when creating tree models. 

Some engines, such as `"xgboost"`, `"xrf"`, and `"lightgbm"`, interpret their analogue to the `mtry` argument as the _proportion_ of predictors that will be randomly sampled at each split rather than the _count_. In some settings, such as when tuning over preprocessors that influence the number of predictors, this parameterization is quite helpful---interpreting `mtry` as a proportion means that `[0, 1]` is always a valid range for that parameter, regardless of input data.

parsnip and its extensions accommodate this parameterization using the `counts` argument: a logical indicating whether `mtry` should be interpreted as the number of predictors that will be randomly sampled at each split. `TRUE` indicates that `mtry` will be interpreted in its sense as a count, `FALSE` indicates that the argument will be interpreted in its sense as a proportion.

`mtry` is a main model argument for \\code{\\link[=boost_tree]{boost_tree()}} and \\code{\\link[=rand_forest]{rand_forest()}}, and thus should not have an engine-specific interface. So, regardless of engine, `counts` defaults to `TRUE`. For engines that support the proportion interpretation (currently `"xgboost"` and `"xrf"`, via the rules package, and `"lightgbm"` via the bonsai package) the user can pass the `counts = FALSE` argument to `set_engine()` to supply `mtry` values within `[0, 1]`.

## Initializing h2o 


To use the h2o engine with tidymodels, please run `h2o::h2o.init()` first. By default, This connects R to the local h2o server. This needs to be done in every new R session. You can also connect to a remote h2o server with an IP address, for more details see [h2o::h2o.init()]. 

You can control the number of threads in the thread pool used by h2o with the `nthreads` argument. By default, it uses all CPUs on the host. This is different from the usual parallel processing mechanism in tidymodels for tuning, while tidymodels parallelizes over resamples, h2o parallelizes over hyperparameter combinations for a given resample. 

h2o will automatically shut down the local h2o instance started by R when R is terminated. To manually stop the h2o server, run `h2o::h2o.shutdown()`. 

## Saving fitted model objects


Models fitted with this engine may require native serialization methods to be properly saved and/or passed between R sessions. To learn more about preparing fitted models for serialization, see the bundle package.
