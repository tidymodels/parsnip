# Other predict methods.

These are internal functions not meant to be directly called by the
user.

## Usage

``` r
# S3 method for class 'model_fit'
predict_class(object, new_data, ...)

predict_class(object, ...)

# S3 method for class 'model_fit'
predict_classprob(object, new_data, ...)

predict_classprob(object, ...)

# S3 method for class 'model_fit'
predict_hazard(object, new_data, eval_time, time = deprecated(), ...)

predict_hazard(object, ...)

# S3 method for class 'model_fit'
predict_confint(object, new_data, level = 0.95, std_error = FALSE, ...)

predict_confint(object, ...)

predict_predint(object, ...)

# S3 method for class 'model_fit'
predict_predint(object, new_data, level = 0.95, std_error = FALSE, ...)

predict_predint(object, ...)

# S3 method for class 'model_fit'
predict_linear_pred(object, new_data, ...)

predict_linear_pred(object, ...)

# S3 method for class 'model_fit'
predict_numeric(object, new_data, ...)

predict_numeric(object, ...)

# S3 method for class 'model_fit'
predict_quantile(
  object,
  new_data,
  quantile_levels = NULL,
  quantile = deprecated(),
  interval = "none",
  level = 0.95,
  ...
)

predict_quantile(object, ...)

# S3 method for class 'model_fit'
predict_survival(
  object,
  new_data,
  eval_time,
  time = deprecated(),
  interval = "none",
  level = 0.95,
  ...
)

predict_survival(object, ...)

# S3 method for class 'model_fit'
predict_time(object, new_data, ...)

predict_time(object, ...)
```

## Arguments

- object:

  A [model
  fit](https://parsnip.tidymodels.org/dev/reference/model_fit.md).

- new_data:

  A rectangular data object, such as a data frame.

- ...:

  Additional `parsnip`-related options, depending on the value of
  `type`. Arguments to the underlying model's prediction function cannot
  be passed here (use the `opts` argument instead). Possible arguments
  are:

  - `interval`: for `type` equal to `"survival"` or `"quantile"`, should
    interval estimates be added, if available? Options are `"none"` and
    `"confidence"`.

  - `level`: for `type` equal to `"conf_int"`, `"pred_int"`, or
    `"survival"`, this is the parameter for the tail area of the
    intervals (e.g. confidence level for confidence intervals). Default
    value is `0.95`.

  - `std_error`: for `type` equal to `"conf_int"` or `"pred_int"`, add
    the standard error of fit or prediction (on the scale of the linear
    predictors). Default value is `FALSE`.

  - `quantile`: for `type` equal to `quantile`, the quantiles of the
    distribution. Default is `(1:9)/10`.

  - `eval_time`: for `type` equal to `"survival"` or `"hazard"`, the
    time points at which the survival probability or hazard is
    estimated.

- level:

  A single numeric value between zero and one for the interval
  estimates.

- std_error:

  A single logical for whether the standard error should be returned
  (assuming that the model can compute it).

- quantile, quantile_levels:

  A vector of values between 0 and 1 for the quantile to be predicted.
  If the model has a `"quantile regression"` mode, this value should be
  `NULL`. For other modes, the default is `(1:9)/10`. Note that, as of
  version 1.3.0 of parsnip, the `quantile` is deprecated. Use
  `quantile_levels` instead.
