# Display currently available engines for a model

The possible engines for a model can depend on what packages are loaded.
Some parsnip extension add engines to existing models. For example, the
poissonreg package adds additional engines for the
[`poisson_reg()`](https://parsnip.tidymodels.org/reference/poisson_reg.md)
model and these are not available unless poissonreg is loaded.

## Usage

``` r
show_engines(x)
```

## Arguments

- x:

  The name of a parsnip model (e.g., "linear_reg", "mars", etc.)

## Value

A tibble.

## Examples

``` r
show_engines("linear_reg")
#> # A tibble: 8 × 2
#>   engine   mode               
#>   <chr>    <chr>              
#> 1 lm       regression         
#> 2 glm      regression         
#> 3 glmnet   regression         
#> 4 stan     regression         
#> 5 spark    regression         
#> 6 keras    regression         
#> 7 brulee   regression         
#> 8 quantreg quantile regression
```
