# Developer functions for predictions via BART models

Developer functions for predictions via BART models

## Usage

``` r
dbart_predict_calc(obj, new_data, type, level = 0.95, std_err = FALSE)
```

## Arguments

- obj:

  A parsnip object.

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

- level:

  Confidence level.

- std_err:

  Attach column for standard error of prediction or not.
