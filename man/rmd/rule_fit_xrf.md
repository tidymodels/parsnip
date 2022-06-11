


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 8 tuning parameters:

- `mtry`: Proportion Randomly Selected Predictors (type: double, default: see below)

- `trees`: # Trees (type: integer, default: 15L)

- `min_n`: Minimal Node Size (type: integer, default: 1L)

- `tree_depth`: Tree Depth (type: integer, default: 6L)

- `learn_rate`: Learning Rate (type: double, default: 0.3)

- `loss_reduction`: Minimum Loss Reduction (type: double, default: 0.0)

- `sample_size`: Proportion Observations Sampled (type: double, default: 1.0)

- `penalty`: Amount of Regularization (type: double, default: 0.1)


## Translation from parsnip to the underlying model call  (regression)

The **rules** extension package is required to fit this model.


```r
library(rules)

rule_fit(
  mtry = numeric(1),
  trees = integer(1),
  min_n = integer(1),
  tree_depth = integer(1),
  learn_rate = numeric(1),
  loss_reduction = numeric(1),
  sample_size = numeric(1),
  penalty = numeric(1)
) %>%
  set_engine("xrf") %>%
  set_mode("regression") %>%
  translate()
```

```
## RuleFit Model Specification (regression)
## 
## Main Arguments:
##   mtry = numeric(1)
##   trees = integer(1)
##   min_n = integer(1)
##   tree_depth = integer(1)
##   learn_rate = numeric(1)
##   loss_reduction = numeric(1)
##   sample_size = numeric(1)
##   penalty = numeric(1)
## 
## Computational engine: xrf 
## 
## Model fit template:
## rules::xrf_fit(formula = missing_arg(), data = missing_arg(), 
##     colsample_bytree = numeric(1), nrounds = integer(1), min_child_weight = integer(1), 
##     max_depth = integer(1), eta = numeric(1), gamma = numeric(1), 
##     subsample = numeric(1), lambda = numeric(1))
```

## Translation from parsnip to the underlying model call  (classification)

The **rules** extension package is required to fit this model.



```r
library(rules)

rule_fit(
  mtry = numeric(1),
  trees = integer(1),
  min_n = integer(1),
  tree_depth = integer(1),
  learn_rate = numeric(1),
  loss_reduction = numeric(1),
  sample_size = numeric(1),
  penalty = numeric(1)
) %>%
  set_engine("xrf") %>%
  set_mode("classification") %>%
  translate()
```

```
## RuleFit Model Specification (classification)
## 
## Main Arguments:
##   mtry = numeric(1)
##   trees = integer(1)
##   min_n = integer(1)
##   tree_depth = integer(1)
##   learn_rate = numeric(1)
##   loss_reduction = numeric(1)
##   sample_size = numeric(1)
##   penalty = numeric(1)
## 
## Computational engine: xrf 
## 
## Model fit template:
## rules::xrf_fit(formula = missing_arg(), data = missing_arg(), 
##     colsample_bytree = numeric(1), nrounds = integer(1), min_child_weight = integer(1), 
##     max_depth = integer(1), eta = numeric(1), gamma = numeric(1), 
##     subsample = numeric(1), lambda = numeric(1))
```

## Differences from the xrf package

Note that, per the documentation in `?xrf`, transformations of the response variable are not supported. To
use these with `rule_fit()`, we recommend using a recipe instead of the formula method.

Also, there are several configuration differences in how `xrf()` is fit between that package and the wrapper used in **rules**. Some differences in default values are:

| parameter  | **xrf** | **rules** |
|------------|---------|-----------|
| `trees`    |  100    | 15        |
|`max_depth` | 3       | 6         |


These differences will create a disparity in the values of the `penalty` argument that **glmnet** uses. Also, **rules** can also set `penalty` whereas **xrf** uses an internal 5-fold cross-validation to determine it (by default).

## Preprocessing requirements

Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.

## Other details

### Interpreting `mtry`


The `mtry` argument denotes the number of predictors that will be randomly sampled at each split when creating tree models. 

Some engines, such as `"xgboost"`, `"xrf"`, and `"lightgbm"`, interpret their analogue to the `mtry` argument as the _proportion_ of predictors that will be randomly sampled at each split rather than the _count_. In some settings, such as when tuning over preprocessors that influence the number of predictors, this parameterization is quite helpful---interpreting `mtry` as a proportion means that $[0, 1]$ is always a valid range for that parameter, regardless of input data.

parsnip and its extensions accommodate this parameterization using the `counts` argument: a logical indicating whether `mtry` should be interpreted as the number of predictors that will be randomly sampled at each split. `TRUE` indicates that `mtry` will be interpreted in its sense as a count, `FALSE` indicates that the argument will be interpreted in its sense as a proportion.

`mtry` is a main model argument for \\code{\\link[=boost_tree]{boost_tree()}} and \\code{\\link[=rand_forest]{rand_forest()}}, and thus should not have an engine-specific interface. So, regardless of engine, `counts` defaults to `TRUE`. For engines that support the proportion interpretation---currently `"xgboost"`, `"xrf"` (via the rules package), and `"lightgbm"` (via the bonsai package)---the user can pass the `counts = FALSE` argument to `set_engine()` to supply `mtry` values within $[0, 1]$.

### Early stopping


The `stop_iter()`  argument allows the model to prematurely stop training if the objective function does not improve within `early_stop` iterations. 

The best way to use this feature is in conjunction with an _internal validation set_. To do this, pass the `validation` parameter of \\code{\\link[=xgb_train]{xgb_train()}} via the parsnip \\code{\\link[=set_engine]{set_engine()}} function. This is the proportion of the training set that should be reserved for measuring performance (and stopping early). 

If the model specification has `early_stop >= trees`, `early_stop` is converted to `trees - 1` and a warning is issued. 

## Case weights


The underlying model implementation does not allow for case weights. 

## References

 - Friedman and Popescu. "Predictive learning via rule ensembles." Ann. Appl. Stat. 2 (3) 916- 954, September 2008

