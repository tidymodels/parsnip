# Helper functions for checking the penalty of glmnet models

These functions are for developer use.

`.check_glmnet_penalty_fit()` checks that the model specification for
fitting a glmnet model contains a single value.

`.check_glmnet_penalty_predict()` checks that the penalty value used for
prediction is valid. If called by
[`predict()`](https://rdrr.io/r/stats/predict.html), it needs to be a
single value. Multiple values are allowed for
[`multi_predict()`](https://parsnip.tidymodels.org/reference/multi_predict.md).

## Usage

``` r
.check_glmnet_penalty_fit(x, call = rlang::caller_env())

.check_glmnet_penalty_predict(
  penalty = NULL,
  object,
  multi = FALSE,
  call = rlang::caller_env()
)
```

## Arguments

- x:

  An object of class `model_spec`.

- penalty:

  A penalty value to check.

- object:

  An object of class `model_fit`.

- multi:

  A logical indicating if multiple values are allowed.
