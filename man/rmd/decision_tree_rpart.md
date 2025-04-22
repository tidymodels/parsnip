


For this engine, there are multiple modes: classification, regression, and censored regression

## Tuning Parameters



This model has 3 tuning parameters:

- `tree_depth`: Tree Depth (type: integer, default: 30L)

- `min_n`: Minimal Node Size (type: integer, default: 2L)

- `cost_complexity`: Cost-Complexity Parameter (type: double, default: 0.01)

## Translation from parsnip to the original package (classification)


``` r
decision_tree(tree_depth = integer(1), min_n = integer(1), cost_complexity = double(1)) |> 
  set_engine("rpart") |> 
  set_mode("classification") |> 
  translate()
```

```
## Decision Tree Model Specification (classification)
## 
## Main Arguments:
##   cost_complexity = double(1)
##   tree_depth = integer(1)
##   min_n = integer(1)
## 
## Computational engine: rpart 
## 
## Model fit template:
## rpart::rpart(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
##     cp = double(1), maxdepth = integer(1), minsplit = min_rows(0L, 
##         data))
```


## Translation from parsnip to the original package (regression)


``` r
decision_tree(tree_depth = integer(1), min_n = integer(1), cost_complexity = double(1)) |> 
  set_engine("rpart") |> 
  set_mode("regression") |> 
  translate()
```

```
## Decision Tree Model Specification (regression)
## 
## Main Arguments:
##   cost_complexity = double(1)
##   tree_depth = integer(1)
##   min_n = integer(1)
## 
## Computational engine: rpart 
## 
## Model fit template:
## rpart::rpart(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
##     cp = double(1), maxdepth = integer(1), minsplit = min_rows(0L, 
##         data))
```


## Translation from parsnip to the original package (censored regression)

The **censored** extension package is required to fit this model.


``` r
library(censored)

decision_tree(
  tree_depth = integer(1),
  min_n = integer(1),
  cost_complexity = double(1)
) |> 
  set_engine("rpart") |> 
  set_mode("censored regression") |> 
  translate()
```

```
## Decision Tree Model Specification (censored regression)
## 
## Main Arguments:
##   cost_complexity = double(1)
##   tree_depth = integer(1)
##   min_n = integer(1)
## 
## Computational engine: rpart 
## 
## Model fit template:
## pec::pecRpart(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), cp = double(1), maxdepth = integer(1), 
##     minsplit = min_rows(0L, data))
```

## Preprocessing requirements


This engine does not require any special encoding of the predictors. Categorical predictors can be partitioned into groups of factor levels (e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables are not required for this model. 

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

## Other details



Predictions of type `"time"` are predictions of the mean survival time.

## Saving fitted model objects


This model object contains data that are not required to make predictions. When saving the model for the purpose of prediction, the size of the saved object might be substantially reduced by using functions from the [butcher](https://butcher.tidymodels.org) package.

## Examples 

The "Fitting and Predicting with parsnip" article contains [examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#decision-tree-rpart) for `decision_tree()` with the `"rpart"` engine.

## References

-   Kuhn, M, and K Johnson. 2013. *Applied Predictive Modeling*. Springer.

