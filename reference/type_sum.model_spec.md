# Succinct summary of parsnip object

`type_sum` controls how objects are shown when inside tibble columns.

## Usage

``` r
# S3 method for class 'model_spec'
type_sum(x)

# S3 method for class 'model_fit'
type_sum(x)
```

## Arguments

- x:

  A `model_spec` or `model_fit` object to summarise.

## Value

A character value.

## Details

For `model_spec` objects, the summary is "`spec[?]`" or "`spec[+]`". The
former indicates that either the model mode has not been declared or
that the specification has
[`tune()`](https://hardhat.tidymodels.org/reference/tune.html)
parameters. Otherwise, the latter is shown.

For fitted models, either "`fit[x]`" or "`fit[+]`" are used where the
"x" implies that the model fit failed in some way.
