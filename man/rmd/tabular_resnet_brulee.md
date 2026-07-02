


For this engine, there are multiple modes: 

## Tuning Parameters



This model has 0 tuning parameters:



The use of the L1 penalty (a.k.a. the lasso penalty) does _not_ force parameters to be strictly zero (as it does in packages such as glmnet). The zeroing out of parameters is a specific feature the optimization method used in those packages.

## Translation from parsnip to the original package (regression)


``` r
tabular_resnet(
  epochs = integer(1),
  hidden_units = integer(1),
  bottleneck_units = integer(1),
  residual_at = NULL,
  activation = character(1),
  penalty = double(1),
  mixture = double(1),
  dropout = double(1),
  learn_rate = double(1),
  rate_schedule = character(1),
  momentum = double(1),
  batch_size = NULL,
  class_weights = NULL,
  stop_iter = integer(1)
) |>
  set_engine("brulee") |>
  set_mode("regression") |>
  translate()
```

```
## ! parsnip could not locate an implementation for `tabular_resnet` regression model
##   specifications using the `brulee` engine.
## i The parsnip extension package tabby implements support for this specification.
## i Please install (if needed) and load to continue.
```

```
## tabular resnet Model Specification (regression)
## 
## Main Arguments:
##   hidden_units = integer(1)
##   bottleneck_units = integer(1)
##   penalty = double(1)
##   mixture = double(1)
##   dropout = double(1)
##   epochs = integer(1)
##   activation = character(1)
##   learn_rate = double(1)
##   rate_schedule = character(1)
##   momentum = double(1)
##   stop_iter = integer(1)
## 
## Computational engine: brulee
```

Note that parsnip automatically sets linear activation in the last layer.

## Translation from parsnip to the original package (classification)


``` r
tabular_resnet(
  epochs = integer(1),
  hidden_units = integer(1),
  bottleneck_units = integer(1),
  residual_at = NULL,
  activation = character(1),
  penalty = double(1),
  mixture = double(1),
  dropout = double(1),
  learn_rate = double(1),
  rate_schedule = character(1),
  momentum = double(1),
  batch_size = NULL,
  class_weights = NULL,
  stop_iter = integer(1)
) |>
  set_engine("brulee") |>
  set_mode("classification") |>
  translate()
```

```
## ! parsnip could not locate an implementation for `tabular_resnet` classification
##   model specifications using the `brulee` engine.
## i The parsnip extension package tabby implements support for this specification.
## i Please install (if needed) and load to continue.
```

```
## tabular resnet Model Specification (classification)
## 
## Main Arguments:
##   hidden_units = integer(1)
##   bottleneck_units = integer(1)
##   penalty = double(1)
##   mixture = double(1)
##   dropout = double(1)
##   epochs = integer(1)
##   activation = character(1)
##   learn_rate = double(1)
##   rate_schedule = character(1)
##   momentum = double(1)
##   stop_iter = integer(1)
## 
## Computational engine: brulee
```

## Preprocessing requirements

`brulee_resnet()` requires numeric predictors. Factor or categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) before fitting; parsnip does _not_ create indicator variables for this engine, so use a recipe (or some other method) to make them numeric.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.

## Case weights


The underlying model implementation does not allow for case weights. 

## Prediction types


``` r
parsnip:::get_from_env("tabular_resnet_predict") |>
  dplyr::filter(engine == "brulee") |>
  dplyr::select(mode, type)
```

```
## # A tibble: 0 x 2
## # i 2 variables: mode <chr>, type <chr>
```

## References

 - Gorishniy, Y., Rubachev, I., Khrulkov, V., & Babenko, A. (2021). Revisiting deep learning models for tabular data. _Advances in Neural Information Processing Systems_, 34.
