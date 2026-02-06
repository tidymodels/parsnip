


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


## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

Note that the data passed to the `case.weights` column are not used for traditional case weights (where the objective function is multiplied by a row-specific weight). From `?randomForest::randomForest`: "A vector of length same as`y` that are positive weights used only in sampling data to grow each tree (not used in any other calculation)."

They function as sampling weights. 


## Saving fitted model objects


This model object contains data that are not required to make predictions. When saving the model for the purpose of prediction, the size of the saved object might be substantially reduced by using functions from the [butcher](https://butcher.tidymodels.org) package.

## Prediction types


``` r
parsnip:::get_from_env("rand_forest_predict") |>
  dplyr::filter(engine == "randomForest") |>
  dplyr::select(mode, type)
```

```
## # A tibble: 5 x 2
##   mode           type   
##   <chr>          <chr>  
## 1 regression     numeric
## 2 regression     raw    
## 3 classification class  
## 4 classification prob   
## 5 classification raw
```

## Examples 

The "Fitting and Predicting with parsnip" [article](https://www.tidymodels.org/learn/models/parsnip-predictions/) contains examples for `rand_forest()` with the `"randomForest"` engine.

## References

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.

