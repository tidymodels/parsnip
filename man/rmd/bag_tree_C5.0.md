


For this engine, there is a single mode: classification

## Tuning Parameters



This model has 1 tuning parameters:

- `min_n`: Minimal Node Size (type: integer, default: 2L)

## Translation from parsnip to the original package (classification)

The **baguette** extension package is required to fit this model.


``` r
library(baguette)

bag_tree(min_n = integer()) |> 
  set_engine("C5.0") |> 
  set_mode("classification") |> 
  translate()
```

```
## Bagged Decision Tree Model Specification (classification)
## 
## Main Arguments:
##   cost_complexity = 0
##   min_n = integer()
## 
## Computational engine: C5.0 
## 
## Model fit template:
## baguette::bagger(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     minCases = integer(), base_model = "C5.0")
```

## Preprocessing requirements


This engine does not require any special encoding of the predictors. Categorical predictors can be partitioned into groups of factor levels (e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables are not required for this model. 

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 


## References

 - Breiman, L. 1996. "Bagging predictors". Machine Learning. 24 (2): 123-140
 
 - Kuhn, M, and K Johnson. 2013. *Applied Predictive Modeling*. Springer.

