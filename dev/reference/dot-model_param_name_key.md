# Translate names of model tuning parameters

This function creates a key that connects the identifiers users make for
tuning parameter names, the standardized parsnip parameter names, and
the argument names to the underlying fit function for the engine.

## Usage

``` r
.model_param_name_key(object, as_tibble = TRUE)
```

## Arguments

- object:

  A workflow or parsnip model specification.

- as_tibble:

  A logical. Should the results be in a tibble (the default) or in a
  list that can facilitate renaming grid objects?

## Value

A tibble with columns `user`, `parsnip`, and `engine`, or a list with
named character vectors `user_to_parsnip` and `parsnip_to_engine`.

## Examples

``` r
mod <-
 linear_reg(penalty = tune("regularization"), mixture = tune()) |>
 set_engine("glmnet")

mod |> .model_param_name_key()
#> # A tibble: 2 × 3
#>   user           parsnip engine
#>   <chr>          <chr>   <chr> 
#> 1 regularization penalty lambda
#> 2 mixture        mixture alpha 

rn <- mod |> .model_param_name_key(as_tibble = FALSE)
rn
#> $user_to_parsnip
#>          penalty          mixture 
#> "regularization"        "mixture" 
#> 
#> $parsnip_to_engine
#>    lambda     alpha 
#> "penalty" "mixture" 
#> 

grid <- tidyr::crossing(regularization = c(0, 1), mixture = (0:3) / 3)

grid |>
  dplyr::rename(!!!rn$user_to_parsnip)
#> # A tibble: 8 × 2
#>   penalty mixture
#>     <dbl>   <dbl>
#> 1       0   0    
#> 2       0   0.333
#> 3       0   0.667
#> 4       0   1    
#> 5       1   0    
#> 6       1   0.333
#> 7       1   0.667
#> 8       1   1    

grid |>
  dplyr::rename(!!!rn$user_to_parsnip) |>
  dplyr::rename(!!!rn$parsnip_to_engine)
#> # A tibble: 8 × 2
#>   lambda alpha
#>    <dbl> <dbl>
#> 1      0 0    
#> 2      0 0.333
#> 3      0 0.667
#> 4      0 1    
#> 5      1 0    
#> 6      1 0.333
#> 7      1 0.667
#> 8      1 1    
```
