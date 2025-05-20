


For this engine, there is a single mode: censored regression

## Tuning Parameters



This model has 5 tuning parameters:

- `mtry`: # Randomly Selected Predictors (type: integer, default: see below)

- `trees`: # Trees (type: integer, default: 100L)

- `tree_depth`: Tree Depth (type: integer, default: 2L)

- `min_n`: Minimal Node Size (type: integer, default: 10L)

- `loss_reduction`: Minimum Loss Reduction (type: double, default: 0)

The `mtry` parameter is related to the number of predictors. The default is to use all predictors.

## Translation from parsnip to the original package (censored regression)

The **censored** extension package is required to fit this model.


``` r
library(censored)

boost_tree() |> 
  set_engine("mboost") |> 
  set_mode("censored regression") |> 
  translate()
```

```
## Boosted Tree Model Specification (censored regression)
## 
## Computational engine: mboost 
## 
## Model fit template:
## censored::blackboost_train(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), family = mboost::CoxPH())
```

`censored::blackboost_train()` is a wrapper around [mboost::blackboost()] (and other functions) that makes it easier to run this model. 

## Preprocessing requirements


This engine does not require any special encoding of the predictors. Categorical predictors can be partitioned into groups of factor levels (e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables are not required for this model. 

## Other details



Predictions of type `"time"` are predictions of the mean survival time.

## References

 - Buehlmann P, Hothorn T. 2007. Boosting algorithms: regularization, prediction and model fitting. _Statistical Science_, 22(4), 477–505.

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.
