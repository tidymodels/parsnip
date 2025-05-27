


For this engine, there are multiple modes: classification, regression, and censored regression

## Tuning Parameters



This model has 4 tuning parameters:

- `class_cost`: Class Cost (type: double, default: (see below))

- `tree_depth`: Tree Depth (type: integer, default: 30L)

- `min_n`: Minimal Node Size (type: integer, default: 2L)

- `cost_complexity`: Cost-Complexity Parameter (type: double, default: 0.01)

For the `class_cost` parameter, the value can be a non-negative scalar for a class cost (where a cost of 1 means no extra cost). This is useful for when the first level of the outcome factor is the minority class. If this is not the case, values between zero and one can be used to bias to the second level of the factor.


## Translation from parsnip to the original package (classification)

The **baguette** extension package is required to fit this model.


``` r
library(baguette)

bag_tree(tree_depth = integer(1), min_n = integer(1), cost_complexity = double(1)) |> 
  set_engine("rpart") |> 
  set_mode("classification") |> 
  translate()
```

```
## Bagged Decision Tree Model Specification (classification)
## 
## Main Arguments:
##   cost_complexity = double(1)
##   tree_depth = integer(1)
##   min_n = integer(1)
## 
## Computational engine: rpart 
## 
## Model fit template:
## baguette::bagger(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), cp = double(1), maxdepth = integer(1), 
##     minsplit = integer(1), base_model = "CART")
```


## Translation from parsnip to the original package (regression)

The **baguette** extension package is required to fit this model.


``` r
library(baguette)

bag_tree(tree_depth = integer(1), min_n = integer(1), cost_complexity = double(1)) |> 
  set_engine("rpart") |> 
  set_mode("regression") |> 
  translate()
```

```
## Bagged Decision Tree Model Specification (regression)
## 
## Main Arguments:
##   cost_complexity = double(1)
##   tree_depth = integer(1)
##   min_n = integer(1)
## 
## Computational engine: rpart 
## 
## Model fit template:
## baguette::bagger(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), cp = double(1), maxdepth = integer(1), 
##     minsplit = integer(1), base_model = "CART")
```

## Translation from parsnip to the original package (censored regression)

The **censored** extension package is required to fit this model.


``` r
library(censored)

bag_tree(tree_depth = integer(1), min_n = integer(1), cost_complexity = double(1)) |> 
  set_engine("rpart") |> 
  set_mode("censored regression") |> 
  translate()
```

```
## Bagged Decision Tree Model Specification (censored regression)
## 
## Main Arguments:
##   cost_complexity = double(1)
##   tree_depth = integer(1)
##   min_n = integer(1)
## 
## Computational engine: rpart 
## 
## Model fit template:
## ipred::bagging(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), cp = double(1), maxdepth = integer(1), 
##     minsplit = integer(1))
```


## Preprocessing requirements


This engine does not require any special encoding of the predictors. Categorical predictors can be partitioned into groups of factor levels (e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables are not required for this model. 

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

## Other details



Predictions of type `"time"` are predictions of the median survival time.

## References

 - Breiman L. 1996. "Bagging predictors". Machine Learning. 24 (2): 123-140
 
 - Hothorn T, Lausen B, Benner A, Radespiel-Troeger M. 2004. Bagging Survival Trees. _Statistics in Medicine_, 23(1), 77–91.
 
 - Kuhn, M, and K Johnson. 2013. *Applied Predictive Modeling*. Springer.
