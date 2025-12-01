# Tidy method for null models

Return the results of `nullmodel` as a tibble

## Usage

``` r
# S3 method for class 'nullmodel'
tidy(x, ...)
```

## Arguments

- x:

  A `nullmodel` object.

- ...:

  Not used.

## Value

A tibble with column `value`.

## Examples

``` r
nullmodel(mtcars[,-1], mtcars$mpg) |> tidy()
#> # A tibble: 1 × 1
#>   value
#>   <dbl>
#> 1  20.1
```
