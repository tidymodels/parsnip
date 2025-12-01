# Turn a parsnip model object into a tidy tibble

This method tidies the model in a parsnip model object, if it exists.

## Usage

``` r
# S3 method for class 'model_fit'
tidy(x, ...)
```

## Arguments

- x:

  An object to be converted into a tidy
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).

- ...:

  Additional arguments to tidying method.

## Value

a tibble
