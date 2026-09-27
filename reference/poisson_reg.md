# Poisson regression models

`poisson_reg()` defines a generalized linear model for count data that
follow a Poisson distribution. This function can fit regression models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`glm`](https://parsnip.tidymodels.org/reference/details_poisson_reg_glm.md)`¹²`

- [`gee`](https://parsnip.tidymodels.org/reference/details_poisson_reg_gee.md)`²`

- [`glmer`](https://parsnip.tidymodels.org/reference/details_poisson_reg_glmer.md)`²`

- [`glmnet`](https://parsnip.tidymodels.org/reference/details_poisson_reg_glmnet.md)`²`

- [`h2o`](https://parsnip.tidymodels.org/reference/details_poisson_reg_h2o.md)`²`

- [`hurdle`](https://parsnip.tidymodels.org/reference/details_poisson_reg_hurdle.md)`²`

- [`stan`](https://parsnip.tidymodels.org/reference/details_poisson_reg_stan.md)`²`

- [`stan_glmer`](https://parsnip.tidymodels.org/reference/details_poisson_reg_stan_glmer.md)`²`

- [`zeroinfl`](https://parsnip.tidymodels.org/reference/details_poisson_reg_zeroinfl.md)`²`

¹ The default engine. ² Requires a parsnip extension package.

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
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.md)
for more on setting the engine, including how to set engine arguments.

The model is not trained or fit until the
[`fit()`](https://parsnip.tidymodels.org/reference/fit.md) function is
used with the data.

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

[`fit()`](https://parsnip.tidymodels.org/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`glm engine details`](https://parsnip.tidymodels.org/reference/details_poisson_reg_glm.md),
[`gee engine details`](https://parsnip.tidymodels.org/reference/details_poisson_reg_gee.md),
[`glmer engine details`](https://parsnip.tidymodels.org/reference/details_poisson_reg_glmer.md),
[`glmnet engine details`](https://parsnip.tidymodels.org/reference/details_poisson_reg_glmnet.md),
[`h2o engine details`](https://parsnip.tidymodels.org/reference/details_poisson_reg_h2o.md),
[`hurdle engine details`](https://parsnip.tidymodels.org/reference/details_poisson_reg_hurdle.md),
[`stan engine details`](https://parsnip.tidymodels.org/reference/details_poisson_reg_stan.md),
[`stan_glmer engine details`](https://parsnip.tidymodels.org/reference/details_poisson_reg_stan_glmer.md),
[`zeroinfl engine details`](https://parsnip.tidymodels.org/reference/details_poisson_reg_zeroinfl.md)
