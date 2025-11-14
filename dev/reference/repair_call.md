# Repair a model call object

When the user passes a formula to
[`fit()`](https://generics.r-lib.org/reference/fit.html) *and* the
underlying model function uses a formula, the call object produced by
[`fit()`](https://generics.r-lib.org/reference/fit.html) may not be
usable by other functions. For example, some arguments may still be
quosures and the `data` portion of the call will not correspond to the
original data.

## Usage

``` r
repair_call(x, data)
```

## Arguments

- x:

  A fitted parsnip model. An error will occur if the underlying model
  does not have a `call` element.

- data:

  A data object that is relevant to the call. In most cases, this is the
  data frame that was given to parsnip for the model fit (i.e., the
  training set data). The name of this data object is inserted into the
  call.

## Value

A modified `parsnip` fitted model.

## Details

`repair_call()` call can adjust the model objects call to be usable by
other functions and methods.

## Examples

``` r
fitted_model <-
  linear_reg() |>
  set_engine("lm", model = TRUE) |>
  fit(mpg ~ ., data = mtcars)

# In this call, note that `data` is not `mtcars` and the `model = ~TRUE`
# indicates that the `model` argument is an rlang quosure.
fitted_model$fit$call
#> stats::lm(formula = mpg ~ ., data = data, model = ~TRUE)

# All better:
repair_call(fitted_model, mtcars)$fit$call
#> stats::lm(formula = mpg ~ ., data = mtcars, model = TRUE)
```
