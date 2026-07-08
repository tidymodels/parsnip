# Chronos-2 pretrained forecasting model

`tabular_chronos()` defines a pretrained time-series forecasting model
that produces quantile (distributional) forecasts. The network has fixed
pretrained weights, so no training is performed; the historical
("context") data is ingested at fit time and the model forecasts a fixed
horizon. This function can fit quantile regression and regression
models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`brulee`](https://parsnip.tidymodels.org/dev/reference/details_tabular_chronos_brulee.md)`¹²`

¹ The default engine. ² Requires a parsnip extension package for
quantile regression and regression.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
tabular_chronos(mode = "unknown", engine = "brulee")
```

## Arguments

- mode:

  A single character string for the type of model. The possible values
  for this model are `"quantile regression"` (the natural mode, which
  returns a
  [`hardhat::quantile_pred()`](https://hardhat.tidymodels.org/reference/quantile_pred.html))
  and `"regression"` (which returns the median point forecast). The mode
  must be set before fitting; for `"quantile regression"` it is set with
  `set_mode("quantile regression", quantile_levels = ...)`.

- engine:

  A single character string specifying what computational engine to use
  for fitting. The only valid value is `"brulee"`.

## Details

Unlike the other models in this package, Chronos-2 is pretrained and has
no tuning parameters. Forecast configuration is supplied through the
engine with
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md),
e.g. `set_engine("brulee", prediction_length = 14)`. The available
engine arguments mirror `brulee::brulee_chronos()`: `prediction_length`,
`id_column`, `timestamp_column`, `model_id`, `revision`, `device`, and
`cache_dir`. The `quantile_levels` are taken from the mode (via
[`set_mode()`](https://parsnip.tidymodels.org/dev/reference/set_args.md))
and forwarded to the fit automatically.

On first use the engine downloads the pretrained weights (about 500MB)
and caches them locally.

The parsnip interface forecasts a **single series**.
[`predict()`](https://rdrr.io/r/stats/predict.html) returns one row per
horizon step (`.pred_quantile` or `.pred`), which cannot unambiguously
represent more than one series; supplying data with multiple `id_column`
values is therefore an error. For multi-series forecasting, call
`brulee::brulee_chronos()` directly, where the id column is retained in
the output.

## References

Ansari, A. F., Shchur, O., Küken, J., Auer, A., Han, B., Mercado, P., et
al. (2025). "Chronos-2: From univariate to universal forecasting."
*arXiv preprint* arXiv:2510.15821.

## See also

[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`brulee engine details`](https://parsnip.tidymodels.org/dev/reference/details_tabular_chronos_brulee.md)
`brulee::brulee_chronos()`

## Examples

``` r
show_engines("tabular_chronos")
#> # A tibble: 0 × 2
#> # ℹ 2 variables: engine <chr>, mode <chr>

# Quantile (distributional) forecast
tabular_chronos() |>
  set_engine("brulee", prediction_length = 14) |>
  set_mode("quantile regression", quantile_levels = (1:9) / 10)
#> ! parsnip could not locate an implementation for `tabular_chronos`
#>   quantile regression model specifications using the `brulee` engine.
#> ℹ The parsnip extension package tabby implements support for this
#>   specification.
#> ℹ Please install (if needed) and load to continue.
#> tabular chronos Model Specification (quantile regression)
#> 
#> Engine-Specific Arguments:
#>   prediction_length = 14
#> 
#> Computational engine: brulee 
#> 
#> Quantile levels: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, and 0.9.

# Median point forecast
tabular_chronos() |>
  set_engine("brulee", prediction_length = 14) |>
  set_mode("regression")
#> ! parsnip could not locate an implementation for `tabular_chronos`
#>   regression model specifications using the `brulee` engine.
#> ℹ The parsnip extension package tabby implements support for this
#>   specification.
#> ℹ Please install (if needed) and load to continue.
#> tabular chronos Model Specification (regression)
#> 
#> Engine-Specific Arguments:
#>   prediction_length = 14
#> 
#> Computational engine: brulee 
#> 
```
