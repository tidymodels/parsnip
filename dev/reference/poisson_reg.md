# Poisson regression models

`poisson_reg()` defines a generalized linear model for count data that
follow a Poisson distribution. This function can fit regression models.

`Rd parsnip:::make_engine_list("poisson_reg")`

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
poisson_reg(
  mode = "regression",
  penalty = NULL,
  mixture = NULL,
  engine = "glm"
)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is "regression".

- penalty:

  A non-negative number representing the total amount of regularization
  (`glmnet` only).

- mixture:

  A number between zero and one (inclusive) giving the proportion of L1
  regularization (i.e. lasso) in the model.

  - `mixture = 1` specifies a pure lasso model,

  - `mixture = 0` specifies a ridge regression model, and

  - `0 < mixture < 1` specifies an elastic net model, interpolating
    lasso and ridge.

  Available for `glmnet` and `spark` only.

- engine:

  A single character string specifying what computational engine to use
  for fitting.

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
    poisson_reg(argument = !!value)

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

`Rd parsnip:::make_seealso_list("poisson_reg")`
