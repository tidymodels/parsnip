


For this engine, there are multiple modes: censored regression, regression, and classification

## Tuning Parameters



This model has 3 tuning parameters:

- `trees`: # Trees (type: integer, default: 500L)

- `min_n`: Minimal Node Size (type: integer, default: 20L)

- `mtry`: # Randomly Selected Predictors (type: integer, default: 5L)

## Translation from parsnip to the original package (regression)

The **bonsai** extension package is required to fit this model.


``` r
library(bonsai)

rand_forest() |> 
  set_engine("partykit") |> 
  set_mode("regression") |> 
  translate()
```

```
## Random Forest Model Specification (regression)
## 
## Computational engine: partykit 
## 
## Model fit template:
## parsnip::cforest_train(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg())
```

## Translation from parsnip to the original package (classification)

The **bonsai** extension package is required to fit this model.


``` r
library(bonsai)

rand_forest() |> 
  set_engine("partykit") |> 
  set_mode("classification") |> 
  translate()
```

```
## Random Forest Model Specification (classification)
## 
## Computational engine: partykit 
## 
## Model fit template:
## parsnip::cforest_train(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg())
```

`parsnip::cforest_train()` is a wrapper around [partykit::cforest()] (and other functions) that makes it easier to run this model. 

# Translation from parsnip to the original package (censored regression)

The **censored** extension package is required to fit this model.


``` r
library(censored)

rand_forest() |> 
  set_engine("partykit") |> 
  set_mode("censored regression") |> 
  translate()
```

```
## Random Forest Model Specification (censored regression)
## 
## Computational engine: partykit 
## 
## Model fit template:
## parsnip::cforest_train(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg())
```

`censored::cond_inference_surv_cforest()` is a wrapper around [partykit::cforest()] (and other functions) that makes it easier to run this model. 


## Preprocessing requirements


This engine does not require any special encoding of the predictors. Categorical predictors can be partitioned into groups of factor levels (e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables are not required for this model. 

## Other details



Predictions of type `"time"` are predictions of the median survival time.

## References

 - [partykit: A Modular Toolkit for Recursive Partytioning in R](https://jmlr.org/papers/v16/hothorn15a.html)

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.
