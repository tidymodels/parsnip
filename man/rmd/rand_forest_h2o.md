


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 3 tuning parameters:

- `trees`: # Trees (type: integer, default: 50L)

- `min_n`: Minimal Node Size (type: integer, default: 1)

- `mtry`: # Randomly Selected Predictors (type: integer, default: see below)

`mtry` depends on the number of columns and the model mode. The default in [h2o::h2o.randomForest()] is `floor(sqrt(ncol(x)))` for classification and `floor(ncol(x)/3)` for regression.

## Translation from parsnip to the original package (regression)

[agua::h2o_train_rf()] is a wrapper around [h2o::h2o.randomForest()]. 


``` r
rand_forest(
  mtry = integer(1),
  trees = integer(1),
  min_n = integer(1)
) |>  
  set_engine("h2o") |> 
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
## Computational engine: h2o 
## 
## Model fit template:
## agua::h2o_train_rf(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     validation_frame = missing_arg(), mtries = integer(1), ntrees = integer(1), 
##     min_rows = integer(1))
```

`min_rows()` and `min_cols()` will adjust the number of neighbors if the chosen value if it is not consistent with the actual data dimensions.

## Translation from parsnip to the original package (classification)


``` r
rand_forest(
  mtry = integer(1),
  trees = integer(1),
  min_n = integer(1)
) |> 
  set_engine("h2o") |> 
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
## Computational engine: h2o 
## 
## Model fit template:
## agua::h2o_train_rf(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     validation_frame = missing_arg(), mtries = integer(1), ntrees = integer(1), 
##     min_rows = integer(1))
```

## Preprocessing requirements


This engine does not require any special encoding of the predictors. Categorical predictors can be partitioned into groups of factor levels (e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables are not required for this model. 


## Initializing h2o 


To use the h2o engine with tidymodels, please run `h2o::h2o.init()` first. By default, This connects R to the local h2o server. This needs to be done in every new R session. You can also connect to a remote h2o server with an IP address, for more details see [h2o::h2o.init()]. 

You can control the number of threads in the thread pool used by h2o with the `nthreads` argument. By default, it uses all CPUs on the host. This is different from the usual parallel processing mechanism in tidymodels for tuning, while tidymodels parallelizes over resamples, h2o parallelizes over hyperparameter combinations for a given resample. 

h2o will automatically shut down the local h2o instance started by R when R is terminated. To manually stop the h2o server, run `h2o::h2o.shutdown()`. 

## Saving fitted model objects


Models fitted with this engine may require native serialization methods to be properly saved and/or passed between R sessions. To learn more about preparing fitted models for serialization, see the bundle package.
