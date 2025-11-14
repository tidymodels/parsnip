# Control the fit function

**\[deprecated\]**

Pass options to the
[`fit.model_spec()`](https://parsnip.tidymodels.org/dev/reference/fit.md)
function to control its output and computations

## Usage

``` r
fit_control(verbosity = 1L, catch = FALSE)
```

## Arguments

- verbosity:

  An integer to control how verbose the output is. For a value of zero,
  no messages or output are shown when packages are loaded or when the
  model is fit. For a value of 1, package loading is quiet but model
  fits can produce output to the screen (depending on if they contain
  their own `verbose`-type argument). For a value of 2 or more, any
  output at all is displayed and the execution time of the fit is
  recorded and printed.

- catch:

  A logical where a value of `TRUE` will evaluate the model inside of
  `try(, silent = TRUE)`. If the model fails, an object is still
  returned (without an error) that inherits the class "try-error".

## Value

An S3 object with class "control_parsnip" that is a named list with the
results of the function call

## Details

`fit_control()` is deprecated in favor of
[`control_parsnip()`](https://parsnip.tidymodels.org/dev/reference/control_parsnip.md).

## Examples

``` r
fit_control(verbosity = 2L)
#> Warning: `fit_control()` was deprecated in parsnip 0.1.8.
#> ℹ Please use `control_parsnip()` instead.
#> parsnip control object
#>  - verbose level 2 
```
