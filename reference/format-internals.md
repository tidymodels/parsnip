# Internal functions that format predictions

These are used to ensure that we have appropriate column names inside of
tibbles.

**\[deprecated\]**

`format_num()` is deprecated. Use `format_predictions(x, "numeric")`
instead.

**\[deprecated\]**

`format_class()` is deprecated. Use `format_predictions(x, "class")`
instead.

**\[deprecated\]**

`format_classprobs()` is deprecated. Use `format_predictions(x, "prob")`
instead.

**\[deprecated\]**

`format_time()` is deprecated. Use `format_predictions(x, "time")`
instead.

**\[deprecated\]**

`format_survival()` is deprecated. Use
`format_predictions(x, "survival")` instead.

**\[deprecated\]**

`format_linear_pred()` is deprecated. Use
`format_predictions(x, "linear_pred")` instead.

**\[deprecated\]**

`format_hazard()` is deprecated. Use `format_predictions(x, "hazard")`
instead.

## Usage

``` r
format_predictions(x, type)

format_num(x)

format_class(x)

format_classprobs(x)

format_time(x)

format_survival(x)

format_linear_pred(x)

format_hazard(x)

ensure_parsnip_format(x, col_name, overwrite = TRUE)
```

## Arguments

- x:

  A data frame or vector (depending on the context and function).

- type:

  A string for the prediction type. One of: `"raw"`, `"numeric"`,
  `"class"`, `"prob"`, `"conf_int"`, `"pred_int"`, `"quantile"`,
  `"time"`, `"survival"`, `"linear_pred"`, or `"hazard"`.

- col_name:

  A string for a prediction column name.

- overwrite:

  A logical for whether to overwrite the column name.

## Value

A tibble
