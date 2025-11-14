# Generalized additive models via mgcv

[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) fits a
generalized linear model with additive smoother terms for continuous
predictors.

## Details

For this engine, there are multiple modes: regression and classification

### Tuning Parameters

This model has 2 tuning parameters:

- `select_features`: Select Features? (type: logical, default: FALSE)

- `adjust_deg_free`: Smoothness Adjustment (type: double, default: 1.0)

### Translation from parsnip to the original package (regression)

    gen_additive_mod(adjust_deg_free = numeric(1), select_features = logical(1)) |>
      set_engine("mgcv") |>
      set_mode("regression") |>
      translate()

    ## GAM Model Specification (regression)
    ##
    ## Main Arguments:
    ##   select_features = logical(1)
    ##   adjust_deg_free = numeric(1)
    ##
    ## Computational engine: mgcv
    ##
    ## Model fit template:
    ## mgcv::gam(formula = missing_arg(), data = missing_arg(), weights = missing_arg(),
    ##     select = logical(1), gamma = numeric(1))

### Translation from parsnip to the original package (classification)

    gen_additive_mod(adjust_deg_free = numeric(1), select_features = logical(1)) |>
      set_engine("mgcv") |>
      set_mode("classification") |>
      translate()

    ## GAM Model Specification (classification)
    ##
    ## Main Arguments:
    ##   select_features = logical(1)
    ##   adjust_deg_free = numeric(1)
    ##
    ## Computational engine: mgcv
    ##
    ## Model fit template:
    ## mgcv::gam(formula = missing_arg(), data = missing_arg(), weights = missing_arg(),
    ##     select = logical(1), gamma = numeric(1), family = stats::binomial(link = "logit"))

### Model fitting

This model should be used with a model formula so that smooth terms can
be specified. For example:

    library(mgcv)
    gen_additive_mod() |>
      set_engine("mgcv") |>
      set_mode("regression") |>
      fit(mpg ~ wt + gear + cyl + s(disp, k = 10), data = mtcars)

    ## parsnip model object
    ##
    ##
    ## Family: gaussian
    ## Link function: identity
    ##
    ## Formula:
    ## mpg ~ wt + gear + cyl + s(disp, k = 10)
    ##
    ## Estimated degrees of freedom:
    ## 7.52  total = 11.52
    ##
    ## GCV score: 4.225228

The smoothness of the terms will need to be manually specified (e.g.,
using `s(x, df = 10)`) in the formula. Tuning can be accomplished using
the `adjust_deg_free` parameter.

When using a workflow, pass the *model formula* to
[`workflows::add_model()`](https://workflows.tidymodels.org/reference/add_model.html)’s
`formula` argument, and a simplified *preprocessing formula* elsewhere.

    spec <-
      gen_additive_mod() |>
      set_engine("mgcv") |>
      set_mode("regression")

    workflow() |>
      add_model(spec, formula = mpg ~ wt + gear + cyl + s(disp, k = 10)) |>
      add_formula(mpg ~ wt + gear + cyl + disp) |>
      fit(data = mtcars) |>
      extract_fit_engine()

    ##
    ## Family: gaussian
    ## Link function: identity
    ##
    ## Formula:
    ## mpg ~ wt + gear + cyl + s(disp, k = 10)
    ##
    ## Estimated degrees of freedom:
    ## 7.52  total = 11.52
    ##
    ## GCV score: 4.225228

To learn more about the differences between these formulas, see
[`?model_formula`](https://parsnip.tidymodels.org/dev/reference/model_formula.md).

### Preprocessing requirements

Factor/categorical predictors need to be converted to numeric values
(e.g., dummy or indicator variables) for this engine. When using the
formula method via
[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md), parsnip
will convert factor columns to indicators.

### Case weights

This model can utilize case weights during model fitting. To use them,
see the documentation in
[case_weights](https://parsnip.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

The [`fit()`](https://generics.r-lib.org/reference/fit.html) and
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) arguments
have arguments called `case_weights` that expect vectors of case
weights.

### Saving fitted model objects

This model object contains data that are not required to make
predictions. When saving the model for the purpose of prediction, the
size of the saved object might be substantially reduced by using
functions from the [butcher](https://butcher.tidymodels.org) package.

### References

- Ross, W. 2021. [*Generalized Additive Models in R: A Free, Interactive
  Course using mgcv*](https://noamross.github.io/gams-in-r-course/)

- Wood, S. 2017. *Generalized Additive Models: An Introduction with R*.
  Chapman and Hall/CRC.
