# Change elements of a model specification

`set_args()` can be used to modify the arguments of a model
specification while `set_mode()` is used to change the model's mode.

## Usage

``` r
set_args(object, ...)

set_mode(object, mode, ...)

# S3 method for class 'model_spec'
set_mode(object, mode, quantile_levels = NULL, ...)
```

## Arguments

- object:

  A [model
  specification](https://parsnip.tidymodels.org/reference/model_spec.md).

- ...:

  One or more named model arguments.

- mode:

  A character string for the model type (e.g. "classification" or
  "regression")

- quantile_levels:

  A vector of values between zero and one (only for the
  `"quantile regression"` mode); otherwise, it is `NULL`. The model uses
  these values to appropriately train quantile regression models to make
  predictions for these values (e.g., `quantile_levels = 0.5` is the
  median).

## Value

An updated model object.

## Details

`set_args()` will replace existing values of the arguments.

## Examples

``` r
rand_forest()
#> Random Forest Model Specification (unknown mode)
#> 
#> Computational engine: ranger 
#> 

rand_forest() |>
  set_args(mtry = 3, importance = TRUE) |>
  set_mode("regression")
#> Random Forest Model Specification (regression)
#> 
#> Main Arguments:
#>   mtry = 3
#> 
#> Engine-Specific Arguments:
#>   importance = TRUE
#> 
#> Computational engine: ranger 
#> 

linear_reg() |>
  set_mode("quantile regression", quantile_levels = c(0.2, 0.5, 0.8))
#> Linear Regression Model Specification (quantile regression)
#> 
#> Computational engine: lm 
#> 
#> Quantile levels: 0.2, 0.5, and 0.8.
```
