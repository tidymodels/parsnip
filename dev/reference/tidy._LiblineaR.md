# tidy methods for LiblineaR models

[`tidy()`](https://generics.r-lib.org/reference/tidy.html) methods for
the various `LiblineaR` models that return the coefficients from the
parsnip model fit.

## Usage

``` r
# S3 method for class '`_LiblineaR`'
tidy(x, ...)
```

## Arguments

- x:

  A fitted parsnip model that used the `LiblineaR` engine.

- ...:

  Not used

## Value

A tibble with columns `term` and `estimate`.
