


For this engine, there is a single mode: classification

## Tuning Parameters




This model has 1 tuning parameter:

- `Laplace`: Laplace Correction (type: double, default: 0.0)

[h2o::h2o.naiveBayes()] provides several engine arguments to deal with imbalances and rare classes: 

- `balance_classes` A logical value controlling over/under-sampling (for imbalanced data). Defaults to `FALSE`.

- `class_sampling_factors` The over/under-sampling ratios per class (in lexicographic order). If not specified, sampling factors will be automatically computed to obtain class balance during training. Require `balance_classes` to be `TRUE`.

- `min_sdev`: The minimum standard deviation to use for observations without enough data, must be greater than 1e-10.

- `min_prob`: The minimum probability to use for observations with not enough data.


## Translation from parsnip to the original package

The **agua** extension package is required to fit this model.

[agua::h2o_train_nb()] is a wrapper around [h2o::h2o.naiveBayes()]. 


``` r
naive_Bayes(Laplace = numeric(0)) |> 
  set_engine("h2o") |> 
  translate()
```

```
## Naive Bayes Model Specification (classification)
## 
## Main Arguments:
##   Laplace = numeric(0)
## 
## Computational engine: h2o 
## 
## Model fit template:
## agua::h2o_train_nb(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     validation_frame = missing_arg(), laplace = numeric(0))
```

## Initializing h2o 


To use the h2o engine with tidymodels, please run `h2o::h2o.init()` first. By default, This connects R to the local h2o server. This needs to be done in every new R session. You can also connect to a remote h2o server with an IP address, for more details see [h2o::h2o.init()]. 

You can control the number of threads in the thread pool used by h2o with the `nthreads` argument. By default, it uses all CPUs on the host. This is different from the usual parallel processing mechanism in tidymodels for tuning, while tidymodels parallelizes over resamples, h2o parallelizes over hyperparameter combinations for a given resample. 

h2o will automatically shut down the local h2o instance started by R when R is terminated. To manually stop the h2o server, run `h2o::h2o.shutdown()`. 

## Saving fitted model objects


Models fitted with this engine may require native serialization methods to be properly saved and/or passed between R sessions. To learn more about preparing fitted models for serialization, see the bundle package.
