


For this engine, there are multiple modes: classification and regression

## Tuning Parameters

This model has no tuning parameters.

Engine arguments of interest 

- `max_runtime_secs` and `max_models`: controls the maximum running time and number of models to build in the automatic process. 

- `exclude_algos` and `include_algos`: a character vector indicating the excluded or included algorithms during model building. To see a full list of supported models, see the details section in [h2o::h2o.automl()].

- `validation`: An integer between 0 and 1 specifying the _proportion_ of training data reserved as validation set. This is used by h2o for performance assessment and potential early stopping. 

## Translation from parsnip to the original package (regression)

[agua::h2o_train_auto()] is a wrapper around [h2o::h2o.automl()]. 


``` r
auto_ml() |>  
  set_engine("h2o") |> 
  set_mode("regression") |> 
  translate()
```

```
## Automatic Machine Learning Model Specification (regression)
## 
## Computational engine: h2o 
## 
## Model fit template:
## agua::h2o_train_auto(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     validation_frame = missing_arg(), verbosity = NULL)
```


## Translation from parsnip to the original package (classification)


``` r
auto_ml() |>  
  set_engine("h2o") |> 
  set_mode("classification") |> 
  translate()
```

```
## Automatic Machine Learning Model Specification (classification)
## 
## Computational engine: h2o 
## 
## Model fit template:
## agua::h2o_train_auto(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     validation_frame = missing_arg(), verbosity = NULL)
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.

## Initializing h2o 


To use the h2o engine with tidymodels, please run `h2o::h2o.init()` first. By default, This connects R to the local h2o server. This needs to be done in every new R session. You can also connect to a remote h2o server with an IP address, for more details see [h2o::h2o.init()]. 

You can control the number of threads in the thread pool used by h2o with the `nthreads` argument. By default, it uses all CPUs on the host. This is different from the usual parallel processing mechanism in tidymodels for tuning, while tidymodels parallelizes over resamples, h2o parallelizes over hyperparameter combinations for a given resample. 

h2o will automatically shut down the local h2o instance started by R when R is terminated. To manually stop the h2o server, run `h2o::h2o.shutdown()`. 

## Saving fitted model objects


Models fitted with this engine may require native serialization methods to be properly saved and/or passed between R sessions. To learn more about preparing fitted models for serialization, see the bundle package.
