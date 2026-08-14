# Engine Details

Engines may have pre-set default arguments when executing the model fit call.
For this type of model, the template of the fit calls are below:

## parsnip


``` r
null_model() |>
  set_engine("parsnip") |>
  set_mode("regression") |>
  translate()
```

```
## Null Model Specification (regression)
##
## Computational engine: parsnip
##
## Model fit template:
## parsnip::nullmodel(x = missing_arg(), y = missing_arg())
```


``` r
null_model() |>
  set_engine("parsnip") |>
  set_mode("quantile regression", quantile_levels = c(0.25, 0.5, 0.75)) |>
  translate()
```

```
## Null Model Specification (quantile regression)
##
## Computational engine: parsnip
##
## Model fit template:
## parsnip::nullmodel(x = missing_arg(), y = missing_arg(), quantile_levels = quantile_levels)
```

```
## Quantile levels: 0.25, 0.5, and 0.75.
```


``` r
null_model() |>
  set_engine("parsnip") |>
  set_mode("classification") |>
  translate()
```

```
## Null Model Specification (classification)
##
## Computational engine: parsnip
##
## Model fit template:
## parsnip::nullmodel(x = missing_arg(), y = missing_arg())
```

## Prediction types


``` r
parsnip:::get_from_env("null_model_predict") |>
  dplyr::filter(engine == "parsnip") |>
  dplyr::select(mode, type)
```

```
## # A tibble: 6 x 2
##   mode                type
##   <chr>               <chr>
## 1 quantile regression quantile
## 2 regression          numeric
## 3 regression          raw
## 4 classification      class
## 5 classification      prob
## 6 classification      raw
```
