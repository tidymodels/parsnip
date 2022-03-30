


For this engine, there is a single mode: censored regression

## Tuning Parameters



This model has 3 tuning parameters:

- `trees`: # Trees (type: integer, default: 500L)

- `min_n`: Minimal Node Size (type: integer, default: 20L)

- `mtry`: # Randomly Selected Predictors (type: integer, default: 5L)

## Translation from parsnip to the original package (censored regression)

The **censored** extension package is required to fit this model.


```r
library(censored)

rand_forest() %>% 
  set_engine("party") %>% 
  set_mode("censored regression") %>% 
  translate()
```

```
## Random Forest Model Specification (censored regression)
## 
## Computational engine: party 
## 
## Model fit template:
## censored::cond_inference_surv_cforest(formula = missing_arg(), 
##     data = missing_arg(), weights = missing_arg())
```

`censored::cond_inference_surv_cforest()` is a wrapper around [party::cforest()] (and other functions) that makes it easier to run this model. 

## Preprocessing requirements


This engine does not require any special encoding of the predictors. Categorical predictors can be partitioned into groups of factor levels (e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables are not required for this model. 

## Other details

The main interface for this model uses the formula method since the model specification typically involved the use of [survival::Surv()]. 


## References

 - Hothorn T, Buhlmann P, Dudoit S, Molinaro A, Van der Laan MJ. 2006. Survival Ensembles. _Biostatistics_, 7(3), 355–373.

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.
