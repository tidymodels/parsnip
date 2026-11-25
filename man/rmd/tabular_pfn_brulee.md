


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 4 tuning parameters:

- `num_estimators`: # Estimators (type: integer, default: see below)

- `softmax_temperature`: Softmax Temperature (type: double, default: see below)

- `balance_probabilities`: Balance Probabilities? (type: logical, default: FALSE)

- `average_before_softmax`: Average Before Softmax? (type: logical, default: FALSE)

The defaults for the first two parameters depend on the version of TabPFN. For example, the number of estimators for many versions is 8 but the 3.5-fast version uses 4. 

TabPFN is a pretrained (prior-data fitted) network; no weights are updated when the model is fit. The training set is ingested at fit time and predictions are made via in-context learning.

Other engine arguments of interest:

 - `training_set_limit`: the maximum number of training set rows used for in-context learning.
 - `device`: The type of chip for the computing (e.g., `"cuda"`, `"mps"`, or `"cpu"`). 

The brulee package uses R torch for the computations so a Python environment is *not* required. The pretrained model weights are downloaded from Hugging Face the first time a model is fit, and the model's license contains provisions for non-commercial use. See [brulee::tab_pfn_download_weights()] for details.

## Translation from parsnip to the original package (regression)


``` r
library(tabby)

tabular_pfn(
  num_estimators = integer(1),
  softmax_temperature = double(1),
  balance_probabilities = logical(1),
  average_before_softmax = logical(1)
) |>
  set_engine("brulee") |>
  set_mode("regression") |>
  translate()
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
## Computational engine: brulee 
## 
## Model fit template:
## brulee::brulee_tab_pfn(formula = missing_arg(), data = missing_arg(), 
##     num_estimators = integer(1), softmax_temperature = double(1), 
##     balance_probabilities = logical(1), average_before_softmax = logical(1))
```

## Translation from parsnip to the original package (classification)


``` r
library(tabby)

tabular_pfn(
  num_estimators = integer(1),
  softmax_temperature = double(1),
  balance_probabilities = logical(1),
  average_before_softmax = logical(1)
) |>
  set_engine("brulee") |>
  set_mode("classification") |>
  translate()
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
## Computational engine: brulee 
## 
## Model fit template:
## brulee::brulee_tab_pfn(formula = missing_arg(), data = missing_arg(), 
##     num_estimators = integer(1), softmax_temperature = double(1), 
##     balance_probabilities = logical(1), average_before_softmax = logical(1))
```

## Preprocessing requirements

Predictors do not require preprocessing; missing values and factor predictors are allowed and are handled internally by the model. There is _no need to pre-encode factors as indicators_ or to scale the predictors.

## Case weights


The underlying model implementation does not allow for case weights. 

## Prediction types


``` r
parsnip:::get_from_env("tabular_pfn_predict") |>
  dplyr::filter(engine == "brulee") |>
  dplyr::select(mode, type)
```

```
## # A tibble: 3 x 2
##   mode           type   
##   <chr>          <chr>  
## 1 classification class  
## 2 classification prob   
## 3 regression     numeric
```

## References

 - Hollmann, N., Müller, S., Purucker, L., Krishnakumar, A., Körfer, M., Hoo, S. B., Schirrmeister, R. T., & Hutter, F. (2025). Accurate predictions on small data with a tabular foundation model. _Nature_, 637(8045), 319-326.

 - Hollmann, N., Müller, S., Eggensperger, K., & Hutter, F. (2022). TabPFN: A transformer that solves small tabular classification problems in a second. _arXiv preprint_ arXiv:2207.01848.

 - Müller, S., Hollmann, N., Pineda Arango, S., Grabocka, J., & Hutter, F. (2021). Transformers can do Bayesian inference. _arXiv preprint_ arXiv:2112.10510.
