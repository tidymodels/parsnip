# sparse tibble can be passed to `fit() - supported

    Code
      xgb_fit <- fit(spec, avg_price_per_room ~ ., data = hotel_data)
    Condition
      Error in `fit()`:
      ! Sparse data cannot be used with formula interface. Please use `fit_xy()` instead.

# sparse tibble can be passed to `fit() - unsupported

    Code
      lm_fit <- fit(spec, avg_price_per_room ~ ., data = hotel_data[1:100, ])
    Condition
      Warning:
      `data` is a sparse tibble, but `linear_reg()` with engine "lm" doesn't accept that. Converting to non-sparse.

# sparse matrix can be passed to `fit() - supported

    Code
      xgb_fit <- fit(spec, avg_price_per_room ~ ., data = hotel_data)
    Condition
      Error in `fit()`:
      ! Sparse data cannot be used with formula interface. Please use `fit_xy()` instead.

# sparse matrix can be passed to `fit() - unsupported

    Code
      lm_fit <- fit(spec, avg_price_per_room ~ ., data = hotel_data[1:100, ])
    Condition
      Warning:
      `data` is a sparse tibble, but `linear_reg()` with engine "lm" doesn't accept that. Converting to non-sparse.

# sparse tibble can be passed to `fit_xy() - unsupported

    Code
      lm_fit <- fit_xy(spec, x = hotel_data[1:100, -1], y = hotel_data[1:100, 1])
    Condition
      Warning:
      `x` is a sparse tibble, but `linear_reg()` with engine "lm" doesn't accept that. Converting to non-sparse.

# sparse matrices can be passed to `fit_xy() - unsupported

    Code
      lm_fit <- fit_xy(spec, x = hotel_data[1:100, -1], y = hotel_data[1:100, 1])
    Condition
      Error in `fit_xy()`:
      ! `x` is a sparse matrix, but `linear_reg()` with engine "lm" doesn't accept that.

# sparse tibble can be passed to `predict() - unsupported

    Code
      preds <- predict(lm_fit, sparse_mtcars)
    Condition
      Warning:
      `x` is a sparse tibble, but `linear_reg()` with engine "lm" doesn't accept that. Converting to non-sparse.

# sparse matrices can be passed to `predict() - unsupported

    Code
      predict(lm_fit, sparse_mtcars)
    Condition
      Error in `predict()`:
      ! `x` is a sparse matrix, but `linear_reg()` with engine "lm" doesn't accept that.

# sparse data work with xgboost engine

    Code
      xgb_fit <- fit(spec, avg_price_per_room ~ ., data = hotel_data)
    Condition
      Error in `fit()`:
      ! Sparse data cannot be used with formula interface. Please use `fit_xy()` instead.

# to_sparse_data_frame() is used correctly

    Code
      fit_xy(spec, x = mtcars[, -1], y = mtcars[, 1])
    Condition
      Error in `to_sparse_data_frame()`:
      ! x is not sparse

---

    Code
      fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])
    Condition
      Error in `to_sparse_data_frame()`:
      ! x is spare, and sparse is not allowed

---

    Code
      fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])
    Condition
      Error in `to_sparse_data_frame()`:
      ! x is spare, and sparse is allowed

# maybe_sparse_matrix() is used correctly

    Code
      fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])
    Condition
      Error in `maybe_sparse_matrix()`:
      ! sparse vectors detected

---

    Code
      fit_xy(spec, x = mtcars[, -1], y = mtcars[, 1])
    Condition
      Error in `maybe_sparse_matrix()`:
      ! no sparse vectors detected

---

    Code
      fit_xy(spec, x = as.data.frame(mtcars)[, -1], y = as.data.frame(mtcars)[, 1])
    Condition
      Error in `maybe_sparse_matrix()`:
      ! no sparse vectors detected

---

    Code
      fit_xy(spec, x = tibble::as_tibble(mtcars)[, -1], y = tibble::as_tibble(mtcars)[,
        1])
    Condition
      Error in `maybe_sparse_matrix()`:
      ! no sparse vectors detected

# we don't run as.matrix() on sparse matrix for glmnet pred #1210

    Code
      predict(lm_fit, hotel_data)
    Condition
      Error in `predict.elnet()`:
      ! data is sparse

# fit() errors if sparse matrix has no colnames

    Code
      fit(spec, avg_price_per_room ~ ., data = hotel_data)
    Condition
      Error in `fit()`:
      ! `x` must have column names.

