


For this engine, there are multiple modes: 

## Tuning Parameters

This model has no tuning parameters. Chronos-2 is a pretrained forecasting model with fixed weights: no training is performed, the historical ("context") data is ingested at fit time, and the model forecasts a fixed horizon. On first use, the engine downloads the pretrained weights (about 500MB) and caches them locally.

Forecast configuration is supplied through engine arguments that mirror [brulee::brulee_chronos()]:

 - `prediction_length`: the number of future time steps to forecast.
 - `id_column`: the name of the column that identifies each series.
 - `timestamp_column`: the name of the column with the time values.
 - `model_id` and `revision`: the pretrained model to use and its revision.
 - `device`: the torch device to use (e.g., `"cpu"`).
 - `cache_dir`: where the pretrained weights are cached.

The `quantile_levels` are taken from the mode (via [parsnip::set_mode()]) and forwarded to the fit automatically. **Note** that the underlying model only allows a specific set of 21 quantile levels: (0.01, 0.05, ..., 0.95, 0.99). 

## Translation from parsnip to the original package (quantile regression)


``` r
tabular_chronos() |>
  set_engine("brulee", prediction_length = 14) |>
  set_mode("quantile regression", quantile_levels = (1:9) / 10) |>
  translate()
```

```
## ! parsnip could not locate an implementation for `tabular_chronos` quantile
##   regression model specifications using the `brulee` engine.
## i The parsnip extension package tabby implements support for this specification.
## i Please install (if needed) and load to continue.
```

```
## tabular chronos Model Specification (quantile regression)
## 
## Engine-Specific Arguments:
##   prediction_length = 14
## 
## Computational engine: brulee
```

```
## Quantile levels: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, and 0.9.
```

## Translation from parsnip to the original package (regression)

The regression mode returns the median point forecast.


``` r
tabular_chronos() |>
  set_engine("brulee", prediction_length = 14) |>
  set_mode("regression") |>
  translate()
```

```
## ! parsnip could not locate an implementation for `tabular_chronos` regression model
##   specifications using the `brulee` engine.
## i The parsnip extension package tabby implements support for this specification.
## i Please install (if needed) and load to continue.
```

```
## tabular chronos Model Specification (regression)
## 
## Engine-Specific Arguments:
##   prediction_length = 14
## 
## Computational engine: brulee
```

## Preprocessing requirements

There are no preprocessing requirements; the data are used as the historical context for the forecast. **However**, any date column that is passed to the data is converted to a numeric value (similar to how `as.numeric(date)` world do). It might be beneficial to pass factors or indicators for cyclic characteristics of the date like year, week, day of the week etc. 

The parsnip interface forecasts a _single series_: `predict()` returns one row per horizon step, which cannot unambiguously represent more than one series, so supplying data with multiple `id_column` values is an error. For multi-series forecasting, call [brulee::brulee_chronos()] directly.

## Case weights


The underlying model implementation does not allow for case weights. 

## Prediction types


``` r
parsnip:::get_from_env("tabular_chronos_predict") |>
  dplyr::filter(engine == "brulee") |>
  dplyr::select(mode, type)
```

```
## # A tibble: 0 x 2
## # i 2 variables: mode <chr>, type <chr>
```

## References

 - Ansari, A. F., Shchur, O., Küken, J., Auer, A., Han, B., Mercado, P., et al. (2025). Chronos-2: From univariate to universal forecasting. _arXiv preprint_ arXiv:2510.15821.
