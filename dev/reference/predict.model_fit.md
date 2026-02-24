# Model predictions

Apply a model to create different types of predictions.
[`predict()`](https://rdrr.io/r/stats/predict.html) can be used for all
types of models and uses the "type" argument for more specificity.

## Usage

``` r
# S3 method for class 'model_fit'
predict(object, new_data, type = NULL, opts = list(), ...)

# S3 method for class 'model_fit'
predict_raw(object, new_data, opts = list(), ...)

predict_raw(object, ...)
```

## Arguments

- object:

  A [model
  fit](https://parsnip.tidymodels.org/dev/reference/model_fit.md).

- new_data:

  A rectangular data object, such as a data frame.

- type:

  A single character value or `NULL`. Possible values are:

  - regression: "`numeric`"

  - classification: "`class`", "`prob`"

  - censored regression: "`survival`", "`time`", "`hazard`",
    "`linear_pred`"

  - quantile regression: "`quantile`"

  - interval estimates: "`conf_int`", "`pred_int`"

  - other: "`raw`"

  When `NULL`, [`predict()`](https://rdrr.io/r/stats/predict.html) will
  choose an appropriate value based on the model's mode.

- opts:

  A list of optional arguments to the underlying predict function that
  will be used when `type = "raw"`. The list should not include options
  for the model object or the new data being predicted.

- ...:

  Additional `parsnip`-related options, depending on the value of
  `type`. Arguments to the underlying model's prediction function cannot
  be passed here (use the `opts` argument instead). Possible arguments
  are:

  - `interval`: for `type` equal to `"survival"`, should interval
    estimates be added, if available? Options are `"none"` and
    `"confidence"`.

  - `level`: for `type` equal to `"conf_int"`, `"pred_int"`, or
    `"survival"`, this is the parameter for the tail area of the
    intervals (e.g. confidence level for confidence intervals). Default
    value is `0.95`.

  - `std_error`: for `type` equal to `"conf_int"` or `"pred_int"`, add
    the standard error of fit or prediction (on the scale of the linear
    predictors). Default value is `FALSE`.

  - `eval_time`: for `type` equal to `"survival"` or `"hazard"`, the
    time points at which the survival probability or hazard is
    estimated.

## Value

With the exception of `type = "raw"`, the result of
`predict.model_fit()`

- is a tibble

- has as many rows as there are rows in `new_data`

- has standardized column names, see below:

For `type = "numeric"`, the tibble has a `.pred` column for a single
outcome and `.pred_{Yname}` columns for a multivariate outcome.

For `type = "class"`, the tibble has a `.pred_class` column.

For `type = "prob"`, the tibble has `.pred_{classlevel}` columns.

For `type = "conf_int"` and `type = "pred_int"`, the tibble has
`.pred_lower` and `.pred_upper` columns with an attribute for the
confidence level. In the case where intervals can be produces for class
probabilities (or other non-scalar outputs), the columns are named
`.pred_lower_{classlevel}` and so on.

For `type = "time"`, the tibble has a `.pred_time` column.

For `type = "survival"`, the tibble has a `.pred` column, which is a
list-column. Each list element contains a tibble with columns
`.eval_time` and `.pred_survival` (and perhaps other columns).

For `type = "hazard"`, the tibble has a `.pred` column, which is a
list-column. Each list element contains a tibble with columns
`.eval_time` and `.pred_hazard` (and perhaps other columns).

For `type = "linear_pred"`, the tibble has a `.pred_linear_pred` column.

For `type = "quantile"`, the tibble has a `.pred_quantile` column, which
is a specialized vector type. See
[`hardhat::quantile_pred()`](https://hardhat.tidymodels.org/reference/quantile_pred.html)
for more details.

Using `type = "raw"` with `predict.model_fit()` will return the
unadulterated results of the prediction function.

In the case of Spark-based models, since table columns cannot contain
dots, the same convention is used except 1) no dots appear in names and
2) vectors are never returned but type-specific prediction functions.

When the model fit failed and the error was captured, the
[`predict()`](https://rdrr.io/r/stats/predict.html) function will return
the same structure as above but filled with missing values. This does
not currently work for multivariate models.

## Details

For `type = NULL`, [`predict()`](https://rdrr.io/r/stats/predict.html)
uses

- `type = "numeric"` for regression models,

- `type = "class"` for classification, and

- `type = "time"` for censored regression.

- `type = "quantile"` for quantile regression.

### Interval predictions

When using `type = "conf_int"` and `type = "pred_int"`, the options
`level` and `std_error` can be used. The latter is a logical for an
extra column of standard error values (if available).

### Censored regression predictions

For censored regression, a numeric vector for `eval_time` is required
when survival or hazard probabilities are requested. The time values are
required to be unique, finite, non-missing, and non-negative. The
[`predict()`](https://rdrr.io/r/stats/predict.html) functions will
adjust the values to fit this specification by removing offending points
(with a warning).

`predict.model_fit()` does not require the outcome to be present. For
performance metrics on the predicted survival probability, inverse
probability of censoring weights (IPCW) are required (see the
`tidymodels.org` reference below). Those require the outcome and are
thus not returned by
[`predict()`](https://rdrr.io/r/stats/predict.html). They can be added
via
[`augment.model_fit()`](https://parsnip.tidymodels.org/dev/reference/augment.md)
if `new_data` contains a column with the outcome as a `Surv` object.

Also, when `type = "linear_pred"`, censored regression models will by
default be formatted such that the linear predictor *increases* with
time. This may have the opposite sign as what the underlying model's
[`predict()`](https://rdrr.io/r/stats/predict.html) method produces. Set
`increasing = FALSE` to suppress this behavior.

## References

<https://www.tidymodels.org/learn/statistics/survival-metrics/>

## Examples

``` r
library(dplyr)

lm_model <-
  linear_reg() |>
  set_engine("lm") |>
  fit(mpg ~ ., data = mtcars |> dplyr::slice(11:32))

pred_cars <-
  mtcars |>
  dplyr::slice(1:10) |>
  dplyr::select(-mpg)

predict(lm_model, pred_cars)
#> # A tibble: 10 × 1
#>    .pred
#>    <dbl>
#>  1  23.4
#>  2  23.3
#>  3  27.6
#>  4  21.5
#>  5  17.6
#>  6  21.6
#>  7  13.9
#>  8  21.7
#>  9  25.6
#> 10  17.1

predict(
  lm_model,
  pred_cars,
  type = "conf_int",
  level = 0.90
)
#> # A tibble: 10 × 2
#>    .pred_lower .pred_upper
#>          <dbl>       <dbl>
#>  1       17.9         29.0
#>  2       18.1         28.5
#>  3       24.0         31.3
#>  4       17.5         25.6
#>  5       14.3         20.8
#>  6       17.0         26.2
#>  7        9.65        18.2
#>  8       16.2         27.2
#>  9       14.2         37.0
#> 10       11.5         22.7

predict(
  lm_model,
  pred_cars,
  type = "raw",
  opts = list(type = "terms")
)
#>                            cyl       disp         hp        drat
#> Mazda RX4         -0.001433177 -0.8113275  0.6303467 -0.06120265
#> Mazda RX4 Wag     -0.001433177 -0.8113275  0.6303467 -0.06120265
#> Datsun 710        -0.009315653 -1.3336453  0.8557288 -0.05014798
#> Hornet 4 Drive    -0.001433177  0.1730406  0.6303467  0.12009386
#> Hornet Sportabout  0.006449298  1.1975870 -0.2314083  0.10461733
#> Valiant           -0.001433177 -0.1584303  0.6966356  0.19084372
#> Duster 360         0.006449298  1.1975870 -1.1594522  0.09135173
#> Merc 240D         -0.009315653 -0.9449204  1.2667197 -0.01477305
#> Merc 230          -0.009315653 -1.0041833  0.8292133 -0.06562451
#> Merc 280          -0.001433177 -0.7349888  0.4579957 -0.06562451
#>                           wt      qsec         vs       am        gear
#> Mazda RX4          2.4139815 -1.567729  0.2006406  2.88774  0.02512680
#> Mazda RX4 Wag      1.4488706 -0.736286  0.2006406  2.88774  0.02512680
#> Datsun 710         3.5494061  1.624418 -0.3511210  2.88774  0.02512680
#> Hornet 4 Drive     0.1620561  2.856736 -0.3511210 -2.40645 -0.06700481
#> Hornet Sportabout -0.6895124 -0.736286  0.2006406 -2.40645 -0.06700481
#> Valiant           -0.7652074  4.014817 -0.3511210 -2.40645 -0.06700481
#> Duster 360        -1.1815297 -2.488255  0.2006406 -2.40645 -0.06700481
#> Merc 240D          0.2566748  3.688179 -0.3511210 -2.40645  0.02512680
#> Merc 230           0.4080647  7.993866 -0.3511210 -2.40645  0.02512680
#> Merc 280          -0.6895124  1.164155 -0.3511210 -2.40645  0.02512680
#>                         carb
#> Mazda RX4         -0.2497240
#> Mazda RX4 Wag     -0.2497240
#> Datsun 710         0.4668753
#> Hornet 4 Drive     0.4668753
#> Hornet Sportabout  0.2280089
#> Valiant            0.4668753
#> Duster 360        -0.2497240
#> Merc 240D          0.2280089
#> Merc 230           0.2280089
#> Merc 280          -0.2497240
#> attr(,"constant")
#> [1] 19.96364
```
