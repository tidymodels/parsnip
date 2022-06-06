


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 8 tuning parameters:

- `mtry`: Proportion Randomly Selected Predictors (type: double, default: 1.0)

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
## rules::xrf_fit(object = missing_arg(), data = missing_arg(), 
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
## rules::xrf_fit(object = missing_arg(), data = missing_arg(), 
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

## Case weights


The underlying model implementation does not allow for case weights. 

## References

 - Friedman and Popescu. "Predictive learning via rule ensembles." Ann. Appl. Stat. 2 (3) 916- 954, September 2008

