# Determine varying arguments

**\[deprecated\]**

[`varying_args()`](https://generics.r-lib.org/reference/varying_args.html)
takes a model specification or a recipe and returns a tibble of
information on all possible varying arguments and whether or not they
are actually varying.

The `id` column is determined differently depending on whether a
`model_spec` or a `recipe` is used. For a `model_spec`, the first class
is used. For a `recipe`, the unique step `id` is used.

## Usage

``` r
# S3 method for class 'model_spec'
varying_args(object, full = TRUE, ...)

# S3 method for class 'recipe'
varying_args(object, full = TRUE, ...)

# S3 method for class 'step'
varying_args(object, full = TRUE, ...)
```

## Arguments

- object:

  A `model_spec` or a `recipe`.

- full:

  A single logical. Should all possible varying parameters be returned?
  If `FALSE`, then only the parameters that are actually varying are
  returned.

- ...:

  Not currently used.

## Value

A tibble with columns for the parameter name (`name`), whether it
contains *any* varying value (`varying`), the `id` for the object
(`id`), and the class that was used to call the method (`type`).

## Examples

``` r
# List all possible varying args for the random forest spec
rand_forest() |> varying_args()
#> Warning: `varying_args()` was deprecated in parsnip 0.1.8.
#> ℹ Please use `tune_args()` instead.
#> # A tibble: 3 × 4
#>   name  varying id          type      
#>   <chr> <lgl>   <chr>       <chr>     
#> 1 mtry  FALSE   rand_forest model_spec
#> 2 trees FALSE   rand_forest model_spec
#> 3 min_n FALSE   rand_forest model_spec

# mtry is now recognized as varying
rand_forest(mtry = varying()) |> varying_args()
#> # A tibble: 3 × 4
#>   name  varying id          type      
#>   <chr> <lgl>   <chr>       <chr>     
#> 1 mtry  TRUE    rand_forest model_spec
#> 2 trees FALSE   rand_forest model_spec
#> 3 min_n FALSE   rand_forest model_spec

# Even engine specific arguments can vary
rand_forest() |>
  set_engine("ranger", sample.fraction = varying()) |>
  varying_args()
#> # A tibble: 4 × 4
#>   name            varying id          type      
#>   <chr>           <lgl>   <chr>       <chr>     
#> 1 mtry            FALSE   rand_forest model_spec
#> 2 trees           FALSE   rand_forest model_spec
#> 3 min_n           FALSE   rand_forest model_spec
#> 4 sample.fraction TRUE    rand_forest model_spec

# List only the arguments that actually vary
rand_forest() |>
  set_engine("ranger", sample.fraction = varying()) |>
  varying_args(full = FALSE)
#> # A tibble: 1 × 4
#>   name            varying id          type      
#>   <chr>           <lgl>   <chr>       <chr>     
#> 1 sample.fraction TRUE    rand_forest model_spec

rand_forest() |>
  set_engine(
    "randomForest",
    strata = Class,
    sampsize = varying()
  ) |>
  varying_args()
#> # A tibble: 5 × 4
#>   name     varying id          type      
#>   <chr>    <lgl>   <chr>       <chr>     
#> 1 mtry     FALSE   rand_forest model_spec
#> 2 trees    FALSE   rand_forest model_spec
#> 3 min_n    FALSE   rand_forest model_spec
#> 4 strata   FALSE   rand_forest model_spec
#> 5 sampsize TRUE    rand_forest model_spec
```
