


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 8 tuning parameters:

- `tree_depth`: Tree Depth (type: integer, default: 6L)

- `trees`: # Trees (type: integer, default: 15L)

- `learn_rate`: Learning Rate (type: double, default: 0.3)

- `mtry`: # Randomly Selected Predictors (type: integer, default: see below)

- `min_n`: Minimal Node Size (type: integer, default: 1L)

- `loss_reduction`: Minimum Loss Reduction (type: double, default: 0.0)

- `sample_size`: Proportion Observations Sampled (type: double, default: 1.0)

- `stop_iter`: # Iterations Before Stopping (type: integer, default: Inf)

For `mtry`, the default value of `NULL` translates to using all available columns.

## Translation from parsnip to the original package (regression)


``` r
boost_tree(
  mtry = integer(), trees = integer(), min_n = integer(), tree_depth = integer(),
  learn_rate = numeric(), loss_reduction = numeric(), sample_size = numeric(),
  stop_iter = integer()
) |>
  set_engine("xgboost") |>
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
##   stop_iter = integer()
## 
## Computational engine: xgboost 
## 
## Model fit template:
## parsnip::xgb_train(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     colsample_bynode = integer(), nrounds = integer(), min_child_weight = integer(), 
##     max_depth = integer(), eta = numeric(), gamma = numeric(), 
##     subsample = numeric(), early_stop = integer(), nthread = 1, 
##     verbose = 0)
```

## Translation from parsnip to the original package (classification)


``` r
boost_tree(
  mtry = integer(), trees = integer(), min_n = integer(), tree_depth = integer(),
  learn_rate = numeric(), loss_reduction = numeric(), sample_size = numeric(),
  stop_iter = integer()
) |> 
  set_engine("xgboost") |> 
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
##   stop_iter = integer()
## 
## Computational engine: xgboost 
## 
## Model fit template:
## parsnip::xgb_train(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     colsample_bynode = integer(), nrounds = integer(), min_child_weight = integer(), 
##     max_depth = integer(), eta = numeric(), gamma = numeric(), 
##     subsample = numeric(), early_stop = integer(), nthread = 1, 
##     verbose = 0)
```

[xgb_train()] is a wrapper around [xgboost::xgb.train()] (and other functions) that makes it easier to run this model. 

## Preprocessing requirements

xgboost does not have a means to translate factor predictors to grouped splits. Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via [fit.model_spec()], parsnip will convert factor columns to indicators using a one-hot encoding.

For classification, non-numeric outcomes (i.e., factors) are internally converted to numeric. For binary classification, the `event_level` argument of `set_engine()` can be set to either `"first"` or `"second"` to specify which level should be used as the event. This can be helpful when a watchlist is used to monitor performance from with the xgboost training process.  

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

## Sparse Data


This model can utilize sparse data during model fitting and prediction. Both sparse matrices such as dgCMatrix from the `Matrix` package and sparse tibbles from the `sparsevctrs` package are supported. See [sparse_data] for more information.

## Other details

### Interfacing with the `params` argument

The xgboost function that parsnip indirectly wraps, [xgboost::xgb.train()], takes most arguments via the `params` list argument. To supply engine-specific arguments that are documented in [xgboost::xgb.train()] as arguments to be passed via `params`, supply the list elements directly as named arguments to [set_engine()] rather than as elements in `params`. For example, pass a non-default evaluation metric like this:


``` r
# good
boost_tree() |>
  set_engine("xgboost", eval_metric = "mae")
```

```
## Boosted Tree Model Specification (unknown mode)
## 
## Engine-Specific Arguments:
##   eval_metric = mae
## 
## Computational engine: xgboost
```

...rather than this:


``` r
# bad
boost_tree() |>
  set_engine("xgboost", params = list(eval_metric = "mae"))
```

```
## Boosted Tree Model Specification (unknown mode)
## 
## Engine-Specific Arguments:
##   params = list(eval_metric = "mae")
## 
## Computational engine: xgboost
```

parsnip will then route arguments as needed. In the case that arguments are passed to `params` via [set_engine()], parsnip will warn and re-route the arguments as needed. Note, though, that arguments passed to `params` cannot be tuned.

### Sparse matrices

xgboost requires the data to be in a sparse format. If your predictor data are already in this format, then use [fit_xy.model_spec()] to pass it to the model function. Otherwise, parsnip converts the data to this format. 

### Parallel processing

By default, the model is trained without parallel processing. This can be change by passing the `nthread` parameter to [set_engine()]. However, it is unwise to combine this with external parallel processing when using the \pkg{tune} package. 

### Interpreting `mtry`


The `mtry` argument denotes the number of predictors that will be randomly sampled at each split when creating tree models. 

Some engines, such as `"xgboost"`, `"xrf"`, and `"lightgbm"`, interpret their analogue to the `mtry` argument as the _proportion_ of predictors that will be randomly sampled at each split rather than the _count_. In some settings, such as when tuning over preprocessors that influence the number of predictors, this parameterization is quite helpful---interpreting `mtry` as a proportion means that `[0, 1]` is always a valid range for that parameter, regardless of input data.

parsnip and its extensions accommodate this parameterization using the `counts` argument: a logical indicating whether `mtry` should be interpreted as the number of predictors that will be randomly sampled at each split. `TRUE` indicates that `mtry` will be interpreted in its sense as a count, `FALSE` indicates that the argument will be interpreted in its sense as a proportion.

`mtry` is a main model argument for \\code{\\link[=boost_tree]{boost_tree()}} and \\code{\\link[=rand_forest]{rand_forest()}}, and thus should not have an engine-specific interface. So, regardless of engine, `counts` defaults to `TRUE`. For engines that support the proportion interpretation (currently `"xgboost"` and `"xrf"`, via the rules package, and `"lightgbm"` via the bonsai package) the user can pass the `counts = FALSE` argument to `set_engine()` to supply `mtry` values within `[0, 1]`.

### Early stopping


The `stop_iter()`  argument allows the model to prematurely stop training if the objective function does not improve within `early_stop` iterations. 

The best way to use this feature is in conjunction with an _internal validation set_. To do this, pass the `validation` parameter of \\code{\\link[=xgb_train]{xgb_train()}} via the parsnip \\code{\\link[=set_engine]{set_engine()}} function. This is the proportion of the training set that should be reserved for measuring performance (and stopping early). 

If the model specification has `early_stop >= trees`, `early_stop` is converted to `trees - 1` and a warning is issued. 

Note that, since the `validation` argument provides an alternative interface to `watchlist`, the `watchlist` argument is guarded by parsnip and will be ignored (with a warning) if passed.

### Objective function

parsnip chooses the objective function based on the characteristics of the outcome. To use a different loss, pass the `objective` argument to [set_engine()] directly. 

## Saving fitted model objects


This model object contains data that are not required to make predictions. When saving the model for the purpose of prediction, the size of the saved object might be substantially reduced by using functions from the [butcher](https://butcher.tidymodels.org) package.


Models fitted with this engine may require native serialization methods to be properly saved and/or passed between R sessions. To learn more about preparing fitted models for serialization, see the bundle package.

## Examples 

The "Fitting and Predicting with parsnip" article contains [examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#boost-tree-xgboost) for `boost_tree()` with the `"xgboost"` engine.

## References

 - [XGBoost: A Scalable Tree Boosting System](https://arxiv.org/abs/1603.02754)
 
 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.
