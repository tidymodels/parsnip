# Tools for models that predict on sub-models

`has_multi_predict()` tests to see if an object can make multiple
predictions on submodels from the same object. `multi_predict_args()`
returns the names of the arguments to
[`multi_predict()`](https://parsnip.tidymodels.org/reference/multi_predict.md)
for this model (if any).

## Usage

``` r
has_multi_predict(object, ...)

# Default S3 method
has_multi_predict(object, ...)

# S3 method for class 'model_fit'
has_multi_predict(object, ...)

# S3 method for class 'workflow'
has_multi_predict(object, ...)

multi_predict_args(object, ...)

# Default S3 method
multi_predict_args(object, ...)

# S3 method for class 'model_fit'
multi_predict_args(object, ...)

# S3 method for class 'workflow'
multi_predict_args(object, ...)
```

## Arguments

- object:

  An object to test.

- ...:

  Not currently used.

## Value

`has_multi_predict()` returns single logical value while
`multi_predict_args()` returns a character vector of argument names (or
`NA` if none exist).

## Examples

``` r
lm_model_idea <- linear_reg() |> set_engine("lm")
has_multi_predict(lm_model_idea)
#> [1] FALSE
lm_model_fit <- fit(lm_model_idea, mpg ~ ., data = mtcars)
has_multi_predict(lm_model_fit)
#> [1] FALSE

multi_predict_args(lm_model_fit)
#> [1] NA

library(kknn)

knn_fit <-
  nearest_neighbor(mode = "regression", neighbors = 5) |>
  set_engine("kknn") |>
  fit(mpg ~ ., mtcars)

multi_predict_args(knn_fit)
#> [1] "neighbors"

multi_predict(knn_fit, mtcars[1, -1], neighbors = 1:4)$.pred
#> [[1]]
#> # A tibble: 4 × 2
#>   neighbors .pred
#>       <int> <dbl>
#> 1         1  21  
#> 2         2  21  
#> 3         3  20.9
#> 4         4  21.0
#> 
```
