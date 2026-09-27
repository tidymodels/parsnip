# Proportional hazards regression

`proportional_hazards()` defines a model for the hazard function as a
multiplicative function of covariates times a baseline hazard. This
function can fit censored regression models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`survival`](https://parsnip.tidymodels.org/reference/details_proportional_hazards_survival.md)`¹²`

- [`glmnet`](https://parsnip.tidymodels.org/reference/details_proportional_hazards_glmnet.md)`²`

¹ The default engine. ² Requires a parsnip extension package.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
proportional_hazards(
  mode = "censored regression",
  engine = "survival",
  penalty = NULL,
  mixture = NULL
)
```

## Arguments

- mode:

  A single character string for the prediction outcome mode. The only
  possible value for this model is "censored regression".

- engine:

  A single character string specifying what computational engine to use
  for fitting.

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
    proportional_hazards(argument = !!value)

Since survival models typically involve censoring (and require the use
of [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
objects), the
[`fit.model_spec()`](https://parsnip.tidymodels.org/reference/fit.md)
function will require that the survival model be specified via the
formula interface.

Proportional hazards models include the Cox model.

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

[`fit()`](https://parsnip.tidymodels.org/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`survival engine details`](https://parsnip.tidymodels.org/reference/details_proportional_hazards_survival.md),
[`glmnet engine details`](https://parsnip.tidymodels.org/reference/details_proportional_hazards_glmnet.md)

## Examples

``` r
show_engines("proportional_hazards")
#> # A tibble: 0 × 2
#> # ℹ 2 variables: engine <chr>, mode <chr>

proportional_hazards(mode = "censored regression")
#> ! parsnip could not locate an implementation for
#>   `proportional_hazards` censored regression model specifications.
#> ℹ The parsnip extension package censored implements support for this
#>   specification.
#> ℹ Please install (if needed) and load to continue.
#> Proportional Hazards Model Specification (censored regression)
#> 
#> Computational engine: survival 
#> 
```
