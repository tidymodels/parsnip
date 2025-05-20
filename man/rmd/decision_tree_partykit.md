


For this engine, there are multiple modes: censored regression, regression, and classification

## Tuning Parameters



This model has 2 tuning parameters:

- `tree_depth`: Tree Depth (type: integer, default: see below)

- `min_n`: Minimal Node Size (type: integer, default: 20L)

The `tree_depth` parameter defaults to `0` which means no restrictions are applied to tree depth.

An engine-specific parameter for this model is: 

 * `mtry`: the number of predictors, selected at random, that are evaluated for splitting. The default is to use all predictors.

## Translation from parsnip to the original package (regression)

The **bonsai** extension package is required to fit this model.


``` r
library(bonsai)

decision_tree(tree_depth = integer(1), min_n = integer(1)) |> 
  set_engine("partykit") |> 
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
## Computational engine: partykit 
## 
## Model fit template:
## parsnip::ctree_train(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), maxdepth = integer(1), minsplit = min_rows(0L, 
##         data))
```

## Translation from parsnip to the original package (classification)

The **bonsai** extension package is required to fit this model.


``` r
library(bonsai)

decision_tree(tree_depth = integer(1), min_n = integer(1)) |> 
  set_engine("partykit") |> 
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
## Computational engine: partykit 
## 
## Model fit template:
## parsnip::ctree_train(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), maxdepth = integer(1), minsplit = min_rows(0L, 
##         data))
```

`parsnip::ctree_train()` is a wrapper around [partykit::ctree()] (and other functions) that makes it easier to run this model. 

## Translation from parsnip to the original package (censored regression)

The **censored** extension package is required to fit this model.


``` r
library(censored)

decision_tree(tree_depth = integer(1), min_n = integer(1)) |> 
  set_engine("partykit") |> 
  set_mode("censored regression") |> 
  translate()
```

```
## Decision Tree Model Specification (censored regression)
## 
## Main Arguments:
##   tree_depth = integer(1)
##   min_n = integer(1)
## 
## Computational engine: partykit 
## 
## Model fit template:
## parsnip::ctree_train(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), maxdepth = integer(1), minsplit = min_rows(0L, 
##         data))
```

`censored::cond_inference_surv_ctree()` is a wrapper around [partykit::ctree()] (and other functions) that makes it easier to run this model. 

## Preprocessing requirements


This engine does not require any special encoding of the predictors. Categorical predictors can be partitioned into groups of factor levels (e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables are not required for this model. 

## Other details



Predictions of type `"time"` are predictions of the median survival time.

## References

 - [partykit: A Modular Toolkit for Recursive Partytioning in R](https://jmlr.org/papers/v16/hothorn15a.html)

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.
