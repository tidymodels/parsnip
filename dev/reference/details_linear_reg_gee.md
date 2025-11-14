# Linear regression via generalized estimating equations (GEE)

`gee::gee()` uses generalized least squares to fit different types of
models with errors that are not independent.

## Details

For this engine, there is a single mode: regression

### Tuning Parameters

This model has no formal tuning parameters. It may be beneficial to
determine the appropriate correlation structure to use, but this
typically does not affect the predicted value of the model. It *does*
have an effect on the inferential results and parameter covariance
values.

### Translation from parsnip to the original package

The **multilevelmod** extension package is required to fit this model.

    library(multilevelmod)

    linear_reg() |>
      set_engine("gee") |>
      set_mode("regression") |>
      translate()

    ## Linear Regression Model Specification (regression)
    ##
    ## Computational engine: gee
    ##
    ## Model fit template:
    ## multilevelmod::gee_fit(formula = missing_arg(), data = missing_arg(),
    ##     family = gaussian)

`multilevelmod::gee_fit()` is a wrapper model around `gee::gee()`.

### Preprocessing requirements

There are no specific preprocessing needs. However, it is helpful to
keep the clustering/subject identifier column as factor or character
(instead of making them into dummy variables). See the examples in the
next section.

### Other details

The model cannot accept case weights.

Both `gee:gee()` and `gee:geepack()` specify the id/cluster variable
using an argument `id` that requires a vector. parsnip doesn’t work that
way so we enable this model to be fit using a artificial function
`id_var()` to be used in the formula. So, in the original package, the
call would look like:

    gee(breaks ~ tension, id = wool, data = warpbreaks, corstr = "exchangeable")

With parsnip, we suggest using the formula method when fitting:

    library(tidymodels)

    linear_reg() |>
      set_engine("gee", corstr = "exchangeable") |>
      fit(breaks ~ tension + id_var(wool), data = warpbreaks)

When using tidymodels infrastructure, it may be better to use a
workflow. In this case, you can add the appropriate columns using
`add_variables()` then supply the GEE formula when adding the model:

    library(tidymodels)

    gee_spec <-
      linear_reg() |>
      set_engine("gee", corstr = "exchangeable")

    gee_wflow <-
      workflow() |>
      # The data are included as-is using:
      add_variables(outcomes = breaks, predictors = c(tension, wool)) |>
      add_model(gee_spec, formula = breaks ~ tension + id_var(wool))

    fit(gee_wflow, data = warpbreaks)

The `gee::gee()` function always prints out warnings and output even
when `silent = TRUE`. The parsnip `"gee"` engine, by contrast, silences
all console output coming from `gee::gee()`, even if `silent = FALSE`.

Also, because of issues with the `gee()` function, a supplementary call
to [`glm()`](https://rdrr.io/r/stats/glm.html) is needed to get the rank
and QR decomposition objects so that
[`predict()`](https://rdrr.io/r/stats/predict.html) can be used.

### Case weights

The underlying model implementation does not allow for case weights.

### References

- Liang, K.Y. and Zeger, S.L. (1986) Longitudinal data analysis using
  generalized linear models. *Biometrika*, 73 13–22.

- Zeger, S.L. and Liang, K.Y. (1986) Longitudinal data analysis for
  discrete and continuous outcomes. *Biometrics*, 42 121–130.
