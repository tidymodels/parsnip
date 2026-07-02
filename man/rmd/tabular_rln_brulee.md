


For this engine, there are multiple modes: 

## Tuning Parameters



This model has 0 tuning parameters:



`penalty_average` and `step_rate` are specified on the natural scale but are best tuned on the log10 scale.

## Translation from parsnip to the original package (regression)


``` r
tabular_rln(
  hidden_units = integer(1),
  penalty_type = character(1),
  penalty_average = double(1),
  step_rate = double(1),
  activation = character(1),
  epochs = integer(1),
  learn_rate = double(1),
  rate_schedule = character(1),
  momentum = double(1),
  batch_size = NULL,
  stop_iter = integer(1)
) |>
  set_engine("brulee") |>
  set_mode("regression") |>
  translate()
```

```
## ! parsnip could not locate an implementation for `tabular_rln` regression model
##   specifications using the `brulee` engine.
## i The parsnip extension package tabby implements support for this specification.
## i Please install (if needed) and load to continue.
```

```
## tabular rln Model Specification (regression)
## 
## Main Arguments:
##   hidden_units = integer(1)
##   penalty_type = character(1)
##   penalty_average = double(1)
##   step_rate = double(1)
##   activation = character(1)
##   epochs = integer(1)
##   learn_rate = double(1)
##   rate_schedule = character(1)
##   momentum = double(1)
##   stop_iter = integer(1)
## 
## Computational engine: brulee
```

## Preprocessing requirements

`brulee_rln()` requires numeric predictors. Factor or categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) before fitting; parsnip does _not_ create indicator variables for this engine, so use a recipe (or some other method) to make them numeric.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.

## Case weights


The underlying model implementation does not allow for case weights. 

## Prediction types


``` r
parsnip:::get_from_env("tabular_rln_predict") |>
  dplyr::filter(engine == "brulee") |>
  dplyr::select(mode, type)
```

```
## # A tibble: 0 x 2
## # i 2 variables: mode <chr>, type <chr>
```

## References

 - Shavitt, I., & Segal, E. (2018). Regularization learning networks: Deep learning for tabular datasets. _Advances in Neural Information Processing Systems_, 31, 1379-1389.
