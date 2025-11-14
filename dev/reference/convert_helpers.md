# Helper functions to convert between formula and matrix interface

Functions to take a formula interface and get the resulting objects (y,
x, weights, etc) back or the other way around. The functions are
intended for developer use. For the most part, this emulates the
internals of [`lm()`](https://rdrr.io/r/stats/lm.html) (and also see the
notes at https://developer.r-project.org/model-fitting-functions.html).

`.convert_form_to_xy_fit()` and `.convert_xy_to_form_fit()` are for when
the data are created for modeling. `.convert_form_to_xy_fit()` saves
both the data objects as well as the objects needed when new data are
predicted (e.g. `terms`, etc.).

`.convert_form_to_xy_new()` and `.convert_xy_to_form_new()` are used
when new samples are being predicted and only require the predictors to
be available.

## Usage

``` r
.convert_form_to_xy_fit(
  formula,
  data,
  ...,
  na.action = na.omit,
  indicators = "traditional",
  composition = "data.frame",
  remove_intercept = TRUE,
  call = rlang::caller_env()
)

.convert_form_to_xy_new(
  object,
  new_data,
  na.action = na.pass,
  composition = "data.frame",
  call = rlang::caller_env()
)

.convert_xy_to_form_fit(
  x,
  y,
  weights = NULL,
  y_name = "..y",
  remove_intercept = TRUE,
  call = rlang::caller_env()
)

.convert_xy_to_form_new(object, new_data)
```

## Arguments

- formula:

  An object of class `formula` (or one that can be coerced to that
  class): a symbolic description of the model to be fitted.

- data:

  A data frame containing all relevant variables (e.g. outcome(s),
  predictors, case weights, etc).

- ...:

  Additional arguments passed to
  [`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html).

- na.action:

  A function which indicates what should happen when the data contain
  NAs.

- indicators:

  A string describing whether and how to create indicator/dummy
  variables from factor predictors. Possible options are `"none"`,
  `"traditional"`, and `"one_hot"`.

- composition:

  A string describing whether the resulting `x` and `y` should be
  returned as a `"matrix"` or a `"data.frame"`.

- remove_intercept:

  A logical indicating whether to remove the intercept column after
  [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) is
  finished.

- object:

  A [model
  fit](https://parsnip.tidymodels.org/dev/reference/model_fit.md).

- new_data:

  A rectangular data object, such as a data frame.

- x:

  A matrix, sparse matrix, or data frame of predictors. Only some models
  have support for sparse matrix input. See
  [`parsnip::get_encoding()`](https://parsnip.tidymodels.org/dev/reference/set_new_model.md)
  for details. `x` should have column names.

- y:

  A vector, matrix or data frame of outcome data.

- weights:

  A numeric vector containing the weights.

- y_name:

  A string specifying the name of the outcome.
