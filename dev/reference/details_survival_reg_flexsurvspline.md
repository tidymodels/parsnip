# Flexible parametric survival regression

`flexsurv::flexsurvspline()` fits a flexible parametric survival model.

## Details

For this engine, there is a single mode: censored regression

### Tuning Parameters

This model has one engine-specific tuning parameter:

- `k`: Number of knots in the spline. The default is `k = 0`.

### Translation from parsnip to the original package

The **censored** extension package is required to fit this model.

    library(censored)

    survival_reg() |>
      set_engine("flexsurvspline") |>
      set_mode("censored regression") |>
      translate()

    ## Parametric Survival Regression Model Specification (censored regression)
    ##
    ## Computational engine: flexsurvspline
    ##
    ## Model fit template:
    ## flexsurv::flexsurvspline(formula = missing_arg(), data = missing_arg(),
    ##     weights = missing_arg())

### Other details

The main interface for this model uses the formula method since the
model specification typically involved the use of
[`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html).

For this engine, stratification cannot be specified via
[`survival::strata()`](https://rdrr.io/pkg/survival/man/strata.html),
please see `flexsurv::flexsurvspline()` for alternative specifications.

Predictions of type `"time"` are predictions of the mean survival time.

### Case weights

This model can utilize case weights during model fitting. To use them,
see the documentation in
[case_weights](https://parsnip.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

The [`fit()`](https://generics.r-lib.org/reference/fit.html) and
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) arguments
have arguments called `case_weights` that expect vectors of case
weights.

### Prediction types

    parsnip:::get_from_env("survival_reg_predict") |>
      dplyr::filter(engine == "flexsurvspline") |>
      dplyr::select(mode, type)

    ## # A tibble: 5 x 2
    ##   mode                type
    ##   <chr>               <chr>
    ## 1 censored regression time
    ## 2 censored regression quantile
    ## 3 censored regression hazard
    ## 4 censored regression survival
    ## 5 censored regression linear_pred

### Saving fitted model objects

This model object contains data that are not required to make
predictions. When saving the model for the purpose of prediction, the
size of the saved object might be substantially reduced by using
functions from the [butcher](https://butcher.tidymodels.org) package.

### References

- Jackson, C. 2016. `flexsurv`: A Platform for Parametric Survival
  Modeling in R. *Journal of Statistical Software*, 70(8), 1 - 33.
