# Model predictions across many sub-models

For some models, predictions can be made on sub-models in the model
object.

## Usage

``` r
multi_predict(object, ...)

# Default S3 method
multi_predict(object, ...)

# S3 method for class '`_xgb.Booster`'
multi_predict(object, new_data, type = NULL, trees = NULL, ...)

# S3 method for class '`_C5.0`'
multi_predict(object, new_data, type = NULL, trees = NULL, ...)

# S3 method for class '`_elnet`'
multi_predict(object, new_data, type = NULL, penalty = NULL, ...)

# S3 method for class '`_lognet`'
multi_predict(object, new_data, type = NULL, penalty = NULL, ...)

# S3 method for class '`_multnet`'
multi_predict(object, new_data, type = NULL, penalty = NULL, ...)

# S3 method for class '`_glmnetfit`'
multi_predict(object, new_data, type = NULL, penalty = NULL, ...)

# S3 method for class '`_earth`'
multi_predict(object, new_data, type = NULL, num_terms = NULL, ...)

# S3 method for class '`_torch_mlp`'
multi_predict(object, new_data, type = NULL, epochs = NULL, ...)

# S3 method for class '`_train.kknn`'
multi_predict(object, new_data, type = NULL, neighbors = NULL, ...)
```

## Arguments

- object:

  A [model
  fit](https://parsnip.tidymodels.org/dev/reference/model_fit.md).

- ...:

  Optional arguments to pass to `predict.model_fit(type = "raw")` such
  as `type`.

- new_data:

  A rectangular data object, such as a data frame.

- type:

  A single character value or `NULL`. Possible values are:

  - regression: "`numeric`"

  - classification: "`class`", "`prob`"

  - censored regression: "`survival`", "`time`", "`hazard`",
    "`linear_pred`"

  - quantile regression: "`quantile`"

  - interval estimates: "`conf_int`", "`pred_int`"

  - other: "`raw`"

  When `NULL`, [`predict()`](https://rdrr.io/r/stats/predict.html) will
  choose an appropriate value based on the model's mode.

- trees:

  An integer vector for the number of trees in the ensemble.

- penalty:

  A numeric vector of penalty values.

- num_terms:

  An integer vector for the number of MARS terms to retain.

- epochs:

  An integer vector for the number of training epochs.

- neighbors:

  An integer vector for the number of nearest neighbors.

## Value

A tibble with the same number of rows as the data being predicted. There
is a list-column named `.pred` that contains tibbles with multiple rows
per sub-model. Note that, within the tibbles, the column names follow
the usual standard based on prediction `type` (i.e. `.pred_class` for
`type = "class"` and so on).
