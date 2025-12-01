# tidy methods for glmnet models

[`tidy()`](https://generics.r-lib.org/reference/tidy.html) methods for
the various `glmnet` models that return the coefficients for the
specific penalty value used by the parsnip model fit.

## Usage

``` r
# S3 method for class '`_elnet`'
tidy(x, penalty = NULL, ...)

# S3 method for class '`_lognet`'
tidy(x, penalty = NULL, ...)

# S3 method for class '`_multnet`'
tidy(x, penalty = NULL, ...)

# S3 method for class '`_fishnet`'
tidy(x, penalty = NULL, ...)

# S3 method for class '`_coxnet`'
tidy(x, penalty = NULL, ...)
```

## Arguments

- x:

  A fitted parsnip model that used the `glmnet` engine.

- penalty:

  A *single* numeric value. If none is given, the value specified in the
  model specification is used.

- ...:

  Not used

## Value

A tibble with columns `term`, `estimate`, and `penalty`. When a
multinomial mode is used, an additional `class` column is included.
