# Fit a Model Specification to a Dataset

[`fit()`](https://generics.r-lib.org/reference/fit.html) and
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) take a
model specification, translate the required code by substituting
arguments, and execute the model fit routine.

## Usage

``` r
# S3 method for class 'model_spec'
fit(
  object,
  formula,
  data,
  case_weights = NULL,
  control = control_parsnip(),
  ...
)

# S3 method for class 'model_spec'
fit_xy(object, x, y, case_weights = NULL, control = control_parsnip(), ...)
```

## Arguments

- object:

  An object of class `model_spec` that has a chosen engine (via
  [`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md)).

- formula:

  An object of class `formula` (or one that can be coerced to that
  class): a symbolic description of the model to be fitted.

- data:

  Optional, depending on the interface (see Details below). A data frame
  containing all relevant variables (e.g. outcome(s), predictors, case
  weights, etc). Note: when needed, a *named argument* should be used.

- case_weights:

  An optional classed vector of numeric case weights. This must return
  `TRUE` when
  [`hardhat::is_case_weights()`](https://hardhat.tidymodels.org/reference/is_case_weights.html)
  is run on it. See
  [`hardhat::frequency_weights()`](https://hardhat.tidymodels.org/reference/frequency_weights.html)
  and
  [`hardhat::importance_weights()`](https://hardhat.tidymodels.org/reference/importance_weights.html)
  for examples.

- control:

  A named list with elements `verbosity` and `catch`. See
  [`control_parsnip()`](https://parsnip.tidymodels.org/dev/reference/control_parsnip.md).

- ...:

  Not currently used; values passed here will be ignored. Other options
  required to fit the model should be passed using
  [`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md).

- x:

  A matrix, sparse matrix, or data frame of predictors. Only some models
  have support for sparse matrix input. See
  [`parsnip::get_encoding()`](https://parsnip.tidymodels.org/dev/reference/set_new_model.md)
  for details. `x` should have column names.

- y:

  A vector, matrix or data frame of outcome data.

## Value

A `model_fit` object that contains several elements:

- `lvl`: If the outcome is a factor, this contains the factor levels at
  the time of model fitting.

- `ordered`: If the outcome is a factor, was it an ordered factor?

- `spec`: The model specification object (`object` in the call to `fit`)

- `fit`: when the model is executed without error, this is the model
  object. Otherwise, it is a `try-error` object with the error message.

- `preproc`: any objects needed to convert between a formula and
  non-formula interface (such as the `terms` object)

The return value will also have a class related to the fitted model
(e.g. `"_glm"`) before the base class of `"model_fit"`.

## Details

[`fit()`](https://generics.r-lib.org/reference/fit.html) and
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html)
substitute the current arguments in the model specification into the
computational engine's code, check them for validity, then fit the model
using the data and the engine-specific code. Different model functions
have different interfaces (e.g. formula or `x`/`y`) and these functions
translate between the interface used when
[`fit()`](https://generics.r-lib.org/reference/fit.html) or
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) was
invoked and the one required by the underlying model.

When possible, these functions attempt to avoid making copies of the
data. For example, if the underlying model uses a formula and
[`fit()`](https://generics.r-lib.org/reference/fit.html) is invoked, the
original data are references when the model is fit. However, if the
underlying model uses something else, such as `x`/`y`, the formula is
evaluated and the data are converted to the required format. In this
case, any calls in the resulting model objects reference the temporary
objects used to fit the model.

If the model engine has not been set, the model's default engine will be
used (as discussed on each model page). If the `verbosity` option of
[`control_parsnip()`](https://parsnip.tidymodels.org/dev/reference/control_parsnip.md)
is greater than zero, a warning will be produced.

If you would like to use an alternative method for generating contrasts
when supplying a formula to
[`fit()`](https://generics.r-lib.org/reference/fit.html), set the global
option `contrasts` to your preferred method. For example, you might set
it to:
`options(contrasts = c(unordered = "contr.helmert", ordered = "contr.poly"))`.
See the help page for
[`stats::contr.treatment()`](https://rdrr.io/r/stats/contrast.html) for
more possible contrast types.

For models with `"censored regression"` modes, an additional computation
is executed and saved in the parsnip object. The `censor_probs` element
contains a "reverse Kaplan-Meier" curve that models the probability of
censoring. This may be used later to compute inverse probability
censoring weights for performance measures.

Sparse data is supported, with the use of the `x` argument in
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html). See
`allow_sparse_x` column of
[`get_encoding()`](https://parsnip.tidymodels.org/dev/reference/set_new_model.md)
for sparse input compatibility.

## See also

[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md),
[`control_parsnip()`](https://parsnip.tidymodels.org/dev/reference/control_parsnip.md),
`model_spec`, `model_fit`

## Examples

``` r
# Although `glm()` only has a formula interface, different
# methods for specifying the model can be used

library(dplyr)
library(modeldata)
data("lending_club")

lr_mod <- logistic_reg()

using_formula <-
  lr_mod |>
  set_engine("glm") |>
  fit(Class ~ funded_amnt + int_rate, data = lending_club)

using_xy <-
  lr_mod |>
   set_engine("glm") |>
  fit_xy(x = lending_club[, c("funded_amnt", "int_rate")],
         y = lending_club$Class)

using_formula
#> parsnip model object
#> 
#> 
#> Call:  stats::glm(formula = Class ~ funded_amnt + int_rate, family = stats::binomial, 
#>     data = data)
#> 
#> Coefficients:
#> (Intercept)  funded_amnt     int_rate  
#>   5.131e+00    2.767e-06   -1.586e-01  
#> 
#> Degrees of Freedom: 9856 Total (i.e. Null);  9854 Residual
#> Null Deviance:       4055 
#> Residual Deviance: 3698  AIC: 3704
using_xy
#> parsnip model object
#> 
#> 
#> Call:  stats::glm(formula = ..y ~ ., family = stats::binomial, data = data)
#> 
#> Coefficients:
#> (Intercept)  funded_amnt     int_rate  
#>   5.131e+00    2.767e-06   -1.586e-01  
#> 
#> Degrees of Freedom: 9856 Total (i.e. Null);  9854 Residual
#> Null Deviance:       4055 
#> Residual Deviance: 3698  AIC: 3704
```
