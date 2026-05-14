# Linear regression

`linear_reg()` defines a model that can predict numeric values from
predictors using a linear function. This function can fit regression
models.

`Rd parsnip:::make_engine_list("linear_reg")`

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
linear_reg(mode = "regression", engine = "lm", penalty = NULL, mixture = NULL)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is "regression".

- engine:

  A single character string specifying what computational engine to use
  for fitting. Possible engines are listed below. The default for this
  model is `"lm"`.

- penalty:

  A non-negative number representing the total amount of regularization
  (specific engines only).

- mixture:

  A number between zero and one (inclusive) denoting the proportion of
  L1 regularization (i.e. lasso) in the model.

  - `mixture = 1` specifies a pure lasso model,

  - `mixture = 0` specifies a ridge regression model, and

  - `0 < mixture < 1` specifies an elastic net model, interpolating
    lasso and ridge.

  Available for specific engines only.

## Details

This function only defines what *type* of model is being fit. Once an
engine is specified, the *method* to fit the model is also defined. See
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md)
for more on setting the engine, including how to set engine arguments.

The model is not trained or fit until the
[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md) function
is used with the data.

Each of the arguments in this function other than `mode` and `engine`
are captured as
[quosures](https://rlang.r-lib.org/reference/topic-quosure.html). To
pass values programmatically, use the [injection
operator](https://rlang.r-lib.org/reference/injection-operator.html)
like so:

    value <- 1
    linear_reg(argument = !!value)

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

`Rd parsnip:::make_seealso_list("linear_reg")`

## Examples

``` r
show_engines("linear_reg")
#> # A tibble: 9 × 2
#>   engine   mode               
#>   <chr>    <chr>              
#> 1 lm       regression         
#> 2 glm      regression         
#> 3 glmnet   regression         
#> 4 stan     regression         
#> 5 spark    regression         
#> 6 keras    regression         
#> 7 keras3   regression         
#> 8 brulee   regression         
#> 9 quantreg quantile regression

linear_reg()
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: lm 
#> 
```
