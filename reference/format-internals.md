# Internal functions that format predictions

These are used to ensure that we have appropriate column names inside of
tibbles.

## Usage

``` r
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

- col_name:

  A string for a prediction column name.

- overwrite:

  A logical for whether to overwrite the column name.

## Value

A tibble
