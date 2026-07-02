


For this engine, there are multiple modes: 

## Tuning Parameters



This model has 0 tuning parameters:



TabPFN is a pretrained (prior-data fitted) network; no weights are updated when the model is fit. The training set is ingested at fit time and predictions are made via in-context learning.

Other engine arguments of interest:

 - `training_set_limit`: the maximum number of training set rows used for in-context learning.
 - `control`: a list of options produced by `tabpfn::control_tab_pfn()`.

The tabpfn package runs the Python "tabpfn" library via reticulate, so a working Python environment is required; the package can create one automatically on first use. The pretrained model weights are downloaded from Hugging Face the first time a model is fit, and the model's license contains provisions for non-commercial use. See [tabpfn::tab_pfn()] for details on all of these topics.

## Translation from parsnip to the original package (regression)


``` r
tabular_pfn(
  num_estimators = integer(1),
  softmax_temperature = double(1),
  balance_probabilities = logical(1),
  average_before_softmax = logical(1)
) |>
  set_engine("tabpfn") |>
  set_mode("regression") |>
  translate()
```

```
## ! parsnip could not locate an implementation for `tabular_pfn` regression model
##   specifications using the `tabpfn` engine.
## i The parsnip extension package tabby implements support for this specification.
## i Please install (if needed) and load to continue.
```

```
## tabular pfn Model Specification (regression)
## 
## Main Arguments:
##   num_estimators = integer(1)
##   softmax_temperature = double(1)
##   balance_probabilities = logical(1)
##   average_before_softmax = logical(1)
## 
## Computational engine: tabpfn
```

## Translation from parsnip to the original package (classification)


``` r
tabular_pfn(
  num_estimators = integer(1),
  softmax_temperature = double(1),
  balance_probabilities = logical(1),
  average_before_softmax = logical(1)
) |>
  set_engine("tabpfn") |>
  set_mode("classification") |>
  translate()
```

```
## ! parsnip could not locate an implementation for `tabular_pfn` classification model
##   specifications using the `tabpfn` engine.
## i The parsnip extension package tabby implements support for this specification.
## i Please install (if needed) and load to continue.
```

```
## tabular pfn Model Specification (classification)
## 
## Main Arguments:
##   num_estimators = integer(1)
##   softmax_temperature = double(1)
##   balance_probabilities = logical(1)
##   average_before_softmax = logical(1)
## 
## Computational engine: tabpfn
```

## Preprocessing requirements

Predictors do not require preprocessing; missing values and factor predictors are allowed and are handled internally by the model. There is _no need to pre-encode factors as indicators_ or to scale the predictors.

## Case weights


The underlying model implementation does not allow for case weights. 

## Prediction types


``` r
parsnip:::get_from_env("tabular_pfn_predict") |>
  dplyr::filter(engine == "tabpfn") |>
  dplyr::select(mode, type)
```

```
## # A tibble: 0 x 2
## # i 2 variables: mode <chr>, type <chr>
```

## References

 - Hollmann, N., Müller, S., Purucker, L., Krishnakumar, A., Körfer, M., Hoo, S. B., Schirrmeister, R. T., & Hutter, F. (2025). Accurate predictions on small data with a tabular foundation model. _Nature_, 637(8045), 319-326.

 - Hollmann, N., Müller, S., Eggensperger, K., & Hutter, F. (2022). TabPFN: A transformer that solves small tabular classification problems in a second. _arXiv preprint_ arXiv:2207.01848.

 - Müller, S., Hollmann, N., Pineda Arango, S., Grabocka, J., & Hutter, F. (2021). Transformers can do Bayesian inference. _arXiv preprint_ arXiv:2112.10510.
