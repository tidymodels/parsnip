# For issue #653 we want to be able to re-run the registration code as
# long as the information being registered is the same.


test_that('re-registration of mode', {
  old_val <- get_from_env("bart_modes")
  expect_no_condition(set_model_mode("bart", "classification"))
  new_val <- get_from_env("bart_modes")
  expect_equal(old_val, new_val)
})

test_that('re-registration of engine', {
  old_val <- get_from_env("bart")
  expect_no_condition(
    set_model_engine("bart", mode = "classification", eng = "dbarts")
  )
  new_val <- get_from_env("bart")
  expect_equal(old_val, new_val)
})


test_that('re-registration of package dependencies', {
  old_val <- get_from_env("bart_pkgs")
  expect_no_error(expect_no_warning(
    set_dependency("bart", "dbarts", "dbarts")
  ))
  new_val <- get_from_env("bart_pkgs")
  expect_equal(old_val, new_val)
})

test_that('re-registration of fit information', {
  old_val <- get_from_env("bart_fit")
  expect_no_condition(
    set_fit(
      model = "bart",
      eng = "dbarts",
      mode = "regression",
      value = list(
        interface = "data.frame",
        data = c(x = "x.train", y = "y.train"),
        protect = c("x", "y"),
        func = c(pkg = "dbarts", fun = "bart"),
        defaults = list(verbose = FALSE, keeptrees = TRUE, keepcall = FALSE)
      )
    )
  )
  new_val <- get_from_env("bart_fit")
  expect_equal(old_val, new_val)

  # Fail if newly registered data is different than existing
  # `verbose` option is different here
  expect_snapshot(
    error = TRUE,
    set_fit(
      model = "bart",
      eng = "dbarts",
      mode = "regression",
      value = list(
        interface = "data.frame",
        data = c(x = "x.train", y = "y.train"),
        protect = c("x", "y"),
        func = c(pkg = "dbarts", fun = "bart"),
        defaults = list(verbose = TRUE, keeptrees = TRUE, keepcall = FALSE)
      )
    )
  )
})

test_that('re-registration of encoding information', {
  old_val <- get_from_env("bart_encoding")
  expect_no_condition(
    set_encoding(
      model = "bart",
      eng = "dbarts",
      mode = "regression",
      options = list(
        predictor_indicators = "none",
        compute_intercept = FALSE,
        remove_intercept = FALSE,
        allow_sparse_x = FALSE
      )
    )
  )
  new_val <- get_from_env("bart_encoding")
  expect_equal(old_val, new_val)

  # Fail if newly registered data is different than existing
  # `compute_intercept` option is different here
  expect_snapshot(
    error = TRUE,
    set_encoding(
      model = "bart",
      eng = "dbarts",
      mode = "regression",
      options = list(
        predictor_indicators = "none",
        compute_intercept = TRUE,
        remove_intercept = FALSE,
        allow_sparse_x = FALSE
      )
    )
  )
})


test_that('re-registration of prediction information', {
  old_val <- get_from_env("bart_predict")
  expect_no_condition(
    set_pred(
      model = "bart",
      eng = "dbarts",
      mode = "regression",
      type = "numeric",
      value = list(
        pre = NULL,
        post = NULL,
        func = c(pkg = "parsnip", fun = "dbart_predict_calc"),
        args =
          list(
            obj = quote(object),
            new_data =  quote(new_data),
            type = "numeric"
          )
      )
    )
  )
  new_val <- get_from_env("bart_predict")
  expect_equal(old_val, new_val)

  # Fail if newly registered data is different than existing
  # `type` option is different here
  expect_snapshot(
    error = TRUE,
    set_pred(
      model = "bart",
      eng = "dbarts",
      mode = "regression",
      type = "numeric",
      value = list(
        pre = NULL,
        post = NULL,
        func = c(pkg = "parsnip", fun = "dbart_predict_calc"),
        args =
          list(
            obj = quote(object),
            new_data =  quote(new_data),
            type = "tuba"
          )
      )
    )
  )
})

