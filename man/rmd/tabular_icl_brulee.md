


For this engine, there are multiple modes: 

## Tuning Parameters



This model has 0 tuning parameters:



TabICL is a pretrained (prior-data fitted) network; no weights are updated when the model is fit. The training set is ingested at fit time and predictions are made via in-context learning.

Other engine arguments of interest:

 - `normalization`: a character vector of per-member normalization methods (`"none"` or `"YeoJohnson"`).
 - `training_set_limit`: the maximum number of training set rows used for in-context learning.
 - `device`: the torch device to use (e.g., `"cpu"`).

## Translation from parsnip to the original package (regression)


``` r
tabular_icl(
  num_estimators = integer(1),
  softmax_temperature = double(1)
) |>
  set_engine("brulee") |>
  set_mode("regression") |>
  translate()
```

```
## ! parsnip could not locate an implementation for `tabular_icl` regression model
##   specifications using the `brulee` engine.
## i The parsnip extension package tabby implements support for this specification.
## i Please install (if needed) and load to continue.
```

```
## tabular icl Model Specification (regression)
## 
## Main Arguments:
##   num_estimators = integer(1)
##   softmax_temperature = double(1)
## 
## Computational engine: brulee
```

## Translation from parsnip to the original package (classification)


``` r
tabular_icl(
  num_estimators = integer(1),
  softmax_temperature = double(1)
) |>
  set_engine("brulee") |>
  set_mode("classification") |>
  translate()
```

```
## ! parsnip could not locate an implementation for `tabular_icl` classification model
##   specifications using the `brulee` engine.
## i The parsnip extension package tabby implements support for this specification.
## i Please install (if needed) and load to continue.
```

```
## tabular icl Model Specification (classification)
## 
## Main Arguments:
##   num_estimators = integer(1)
##   softmax_temperature = double(1)
## 
## Computational engine: brulee
```

## Preprocessing requirements

`brulee_tab_icl()` converts each predictor column to a numeric value internally: factor and character columns are ordinal-encoded and numeric columns are standardized (with an optional Yeo-Johnson transformation) inside the model. There is _no need to pre-encode factors as indicators_; a wide one-hot expansion degrades prediction quality for this model. Predictors also do not need to be scaled by the user.

## Case weights


The underlying model implementation does not allow for case weights. 

## Prediction types


``` r
parsnip:::get_from_env("tabular_icl_predict") |>
  dplyr::filter(engine == "brulee") |>
  dplyr::select(mode, type)
```

```
## # A tibble: 0 x 2
## # i 2 variables: mode <chr>, type <chr>
```

## References

 - Qu, J., Holzmüller, D., Varoquaux, G., & Morvan, M. L. (2025). TabICL: A tabular foundation model for in-context learning on large data. _arXiv preprint_ arXiv:2502.05564.

 - Qu, J., Holzmüller, D., Varoquaux, G., & Morvan, M. L. (2026). TabICLv2: A better, faster, scalable, and open tabular foundation model. _arXiv preprint_ arXiv:2602.11139.
