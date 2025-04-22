


For this engine, there is a single mode: classification

## Tuning Parameters



This model has 1 tuning parameters:

- `min_n`: Minimal Node Size (type: integer, default: 2L)

## Translation from parsnip to the original package (classification)


``` r
decision_tree(min_n = integer()) |> 
  set_engine("C5.0") |> 
  set_mode("classification") |> 
  translate()
```

```
## Decision Tree Model Specification (classification)
## 
## Main Arguments:
##   min_n = integer()
## 
## Computational engine: C5.0 
## 
## Model fit template:
## parsnip::C5.0_train(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     minCases = integer(), trials = 1)
```

[C5.0_train()] is a wrapper around [C50::C5.0()] that makes it easier to run this model.

## Preprocessing requirements


This engine does not require any special encoding of the predictors. Categorical predictors can be partitioned into groups of factor levels (e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables are not required for this model. 

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

## Saving fitted model objects


This model object contains data that are not required to make predictions. When saving the model for the purpose of prediction, the size of the saved object might be substantially reduced by using functions from the [butcher](https://butcher.tidymodels.org) package.


## Examples 

The "Fitting and Predicting with parsnip" article contains [examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#decision-tree-C5.0) for `decision_tree()` with the `"C5.0"` engine.

## References

-   Kuhn, M, and K Johnson. 2013. *Applied Predictive Modeling*. Springer.

