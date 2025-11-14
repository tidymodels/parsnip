# Boosted trees via xgboost

`xgb_train()` and `xgb_predict()` are wrappers for `xgboost` tree-based
models where all of the model arguments are in the main function.

## Usage

``` r
xgb_train(
  x,
  y,
  weights = NULL,
  max_depth = 6,
  nrounds = 15,
  eta = 0.3,
  colsample_bynode = NULL,
  colsample_bytree = NULL,
  min_child_weight = 1,
  gamma = 0,
  subsample = 1,
  validation = 0,
  early_stop = NULL,
  counts = TRUE,
  event_level = c("first", "second"),
  ...
)

xgb_predict(object, new_data, ...)
```

## Arguments

- x:

  A data frame or matrix of predictors

- y:

  A vector (factor or numeric) or matrix (numeric) of outcome data.

- max_depth:

  An integer for the maximum depth of the tree.

- nrounds:

  An integer for the number of boosting iterations.

- eta:

  A numeric value between zero and one to control the learning rate.

- colsample_bynode:

  Subsampling proportion of columns for each node within each tree. See
  the `counts` argument below. The default uses all columns.

- colsample_bytree:

  Subsampling proportion of columns for each tree. See the `counts`
  argument below. The default uses all columns.

- min_child_weight:

  A numeric value for the minimum sum of instance weights needed in a
  child to continue to split.

- gamma:

  A number for the minimum loss reduction required to make a further
  partition on a leaf node of the tree

- subsample:

  Subsampling proportion of rows. By default, all of the training data
  are used.

- validation:

  The *proportion* of the data that are used for performance assessment
  and potential early stopping.

- early_stop:

  An integer or `NULL`. If not `NULL`, it is the number of training
  iterations without improvement before stopping. If `validation` is
  used, performance is base on the validation set; otherwise, the
  training set is used.

- counts:

  A logical. If `FALSE`, `colsample_bynode` and `colsample_bytree` are
  both assumed to be *proportions* of the proportion of columns affects
  (instead of counts).

- event_level:

  For binary classification, this is a single string of either `"first"`
  or `"second"` to pass along describing which level of the outcome
  should be considered the "event".

- ...:

  Other options to pass to `xgb.train()` or xgboost's method for
  [`predict()`](https://rdrr.io/r/stats/predict.html).

- new_data:

  A rectangular data object, such as a data frame.

## Value

A fitted `xgboost` object.
