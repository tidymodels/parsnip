# Parametric survival regression

**\[deprecated\]**

This function is deprecated in favor of
[`survival_reg()`](https://parsnip.tidymodels.org/reference/survival_reg.md)
which uses the `"censored regression"` mode.

`surv_reg()` defines a parametric survival model.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
surv_reg(mode = "regression", engine = "survival", dist = NULL)
```

## Arguments

- mode:

  A single character string for the prediction outcome mode. The only
  possible value for this model is "regression".

- engine:

  A single character string specifying what computational engine to use
  for fitting.

- dist:

  A character string for the probability distribution of the outcome.
  The default is "weibull".

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
    surv_reg(argument = !!value)

Since survival models typically involve censoring (and require the use
of [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
objects), the
[`fit.model_spec()`](https://parsnip.tidymodels.org/reference/fit.md)
function will require that the survival model be specified via the
formula interface.

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)
