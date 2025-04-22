


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 3 tuning parameters:

- `mtry`: # Randomly Selected Predictors (type: integer, default: see below)

- `trees`: # Trees (type: integer, default: 500L)

- `min_n`: Minimal Node Size (type: integer, default: see below)

`mtry` depends on the number of columns and the model mode. The default in [randomForest::randomForest()] is `floor(sqrt(ncol(x)))` for classification and `floor(ncol(x)/3)` for regression.

`min_n` depends on the mode. For regression, a value of 5 is the default. For classification, a value of 10 is used. 

## Translation from parsnip to the original package (regression)


``` r
rand_forest(
  mtry = integer(1),
  trees = integer(1),
  min_n = integer(1)
) |>  
  set_engine("randomForest") |> 
  set_mode("regression") |> 
  translate()
```

```
## Random Forest Model Specification (regression)
## 
## Main Arguments:
##   mtry = integer(1)
##   trees = integer(1)
##   min_n = integer(1)
## 
## Computational engine: randomForest 
## 
## Model fit template:
## randomForest::randomForest(x = missing_arg(), y = missing_arg(), 
##     mtry = min_cols(~integer(1), x), ntree = integer(1), nodesize = min_rows(~integer(1), 
##         x))
```

`min_rows()` and `min_cols()` will adjust the number of neighbors if the chosen value if it is not consistent with the actual data dimensions.

## Translation from parsnip to the original package (classification)


``` r
rand_forest(
  mtry = integer(1),
  trees = integer(1),
  min_n = integer(1)
) |> 
  set_engine("randomForest") |> 
  set_mode("classification") |> 
  translate()
```

```
## Random Forest Model Specification (classification)
## 
## Main Arguments:
##   mtry = integer(1)
##   trees = integer(1)
##   min_n = integer(1)
## 
## Computational engine: randomForest 
## 
## Model fit template:
## randomForest::randomForest(x = missing_arg(), y = missing_arg(), 
##     mtry = min_cols(~integer(1), x), ntree = integer(1), nodesize = min_rows(~integer(1), 
##         x))
```

## Preprocessing requirements


This engine does not require any special encoding of the predictors. Categorical predictors can be partitioned into groups of factor levels (e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables are not required for this model. 

## Saving fitted model objects


This model object contains data that are not required to make predictions. When saving the model for the purpose of prediction, the size of the saved object might be substantially reduced by using functions from the [butcher](https://butcher.tidymodels.org) package.


## Examples 

The "Fitting and Predicting with parsnip" article contains [examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#rand-forest-randomForest) for `rand_forest()` with the `"randomForest"` engine.

## References

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.

