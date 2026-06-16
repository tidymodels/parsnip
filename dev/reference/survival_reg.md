# Parametric survival regression

`survival_reg()` defines a parametric survival model. This function can
fit censored regression models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`survival`](https://parsnip.tidymodels.org/dev/reference/details_survival_reg_survival.md)`¹²`

- [`flexsurv`](https://parsnip.tidymodels.org/dev/reference/details_survival_reg_flexsurv.md)`²`

- [`flexsurvspline`](https://parsnip.tidymodels.org/dev/reference/details_survival_reg_flexsurvspline.md)`²`

¹ The default engine. ² Requires a parsnip extension package.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
survival_reg(mode = "censored regression", engine = "survival", dist = NULL)
```

## Arguments

- mode:

  A single character string for the prediction outcome mode. The only
  possible value for this model is "censored regression".

- engine:

  A single character string specifying what computational engine to use
  for fitting.

- dist:

  A character string for the probability distribution of the outcome.
  The default is "weibull".

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
    survival_reg(argument = !!value)

Since survival models typically involve censoring (and require the use
of [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
objects), the
[`fit.model_spec()`](https://parsnip.tidymodels.org/dev/reference/fit.md)
function will require that the survival model be specified via the
formula interface.

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`survival engine details`](https://parsnip.tidymodels.org/dev/reference/details_survival_reg_survival.md),
[`flexsurv engine details`](https://parsnip.tidymodels.org/dev/reference/details_survival_reg_flexsurv.md),
[`flexsurvspline engine details`](https://parsnip.tidymodels.org/dev/reference/details_survival_reg_flexsurvspline.md)

## Examples

``` r
show_engines("survival_reg")
#> # A tibble: 0 × 2
#> # ℹ 2 variables: engine <chr>, mode <chr>

survival_reg(mode = "censored regression", dist = "weibull")
#> ! parsnip could not locate an implementation for `survival_reg`
#>   censored regression model specifications.
#> ℹ The parsnip extension package censored implements support for this
#>   specification.
#> ℹ Please install (if needed) and load to continue.
#> Parametric Survival Regression Model Specification (censored regression)
#> 
#> Main Arguments:
#>   dist = weibull
#> 
#> Computational engine: survival 
#> 
```
