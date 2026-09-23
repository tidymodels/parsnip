test_that("`relax = TRUE` works for glmnet engines", {
  skip_if_not_installed("glmnet")
  # Issue 1069

  relax_fit <- linear_reg(penalty = 0.5, mixture = 1) |>
    set_engine("glmnet", relax = TRUE) |>
    fit(mpg ~ ., data = mtcars)

  expect_s3_class(relax_fit, "model_fit")
  expect_false(is.null(extract_fit_engine(relax_fit)$relaxed))

  # glmnet re-evaluates its recorded call with base `eval()` when relaxing,
  # so no argument may still be a quosure
  expect_all_false(
    purrr::map_lgl(
      as.list(extract_fit_engine(relax_fit)$call),
      rlang::is_quosure
    )
  )

  # more engine arguments surfaced a different failure in the issue
  relax_fit_2 <- linear_reg(penalty = 0.5, mixture = 1) |>
    set_engine("glmnet", relax = TRUE, standardize = FALSE) |>
    fit(mpg ~ ., data = mtcars)

  expect_false(is.null(extract_fit_engine(relax_fit_2)$relaxed))

  # and via the matrix interface
  relax_fit_xy <- linear_reg(penalty = 0.5, mixture = 1) |>
    set_engine("glmnet", relax = TRUE) |>
    fit_xy(x = mtcars[, -1], y = mtcars$mpg)

  expect_false(is.null(extract_fit_engine(relax_fit_xy)$relaxed))
})

test_that("`multi_predict(type = 'raw')` passes through for glmnet engines", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("modeldata")
  # Issue 857

  hpc <- hpc_data[1:150, c(2:5, 8)]
  lending_club <- lending_club[1:200, ]
  pen <- c(0.1, 0.5)

  # linear_reg: previously the type was silently ignored
  lin_fit <- linear_reg(penalty = 0.123) |>
    set_engine("glmnet") |>
    fit(input_fields ~ log(compounds) + class, data = hpc)
  lin_raw <- multi_predict(lin_fit, hpc[1:5, ], type = "raw", penalty = pen)
  expect_true(is.matrix(lin_raw))
  expect_equal(dim(lin_raw), c(5L, length(pen)))

  # logistic_reg: previously errored in the parsnip formatting helper
  log_fit <- suppressWarnings(
    logistic_reg(penalty = 0.123) |>
      set_engine("glmnet") |>
      fit(Class ~ log(funded_amnt) + int_rate + term, data = lending_club)
  )
  log_raw <- multi_predict(
    log_fit,
    lending_club[1:5, ],
    type = "raw",
    penalty = pen
  )
  expect_true(is.matrix(log_raw))
  expect_equal(dim(log_raw), c(5L, length(pen)))

  # multinom_reg: previously errored inside glmnet's `match.arg()`
  mn_fit <- suppressWarnings(
    multinom_reg(penalty = 0.1) |>
      set_engine("glmnet") |>
      fit(class ~ ., data = hpc)
  )
  mn_raw <- multi_predict(mn_fit, hpc[1:5, ], type = "raw", penalty = pen)
  expect_equal(dim(mn_raw), c(5L, nlevels(hpc$class), length(pen)))

  # other types still return the nested `.pred` tibble
  expect_named(multi_predict(lin_fit, hpc[1:5, ], penalty = pen), ".pred")
  expect_named(
    multi_predict(log_fit, lending_club[1:5, ], type = "prob", penalty = pen),
    ".pred"
  )
  expect_named(
    multi_predict(mn_fit, hpc[1:5, ], type = "class", penalty = pen),
    ".pred"
  )
})

test_that("glmnet predictions work without the per-type wrappers", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("modeldata")
  # Issue 878

  hpc <- hpc_data[1:150, c(2:5, 8)]

  lin_fit <- linear_reg(penalty = 0.123) |>
    set_engine("glmnet") |>
    fit(input_fields ~ log(compounds) + class, data = hpc)
  mn_fit <- suppressWarnings(
    multinom_reg(penalty = 0.1) |>
      set_engine("glmnet") |>
      fit(class ~ ., data = hpc)
  )

  # the exported generics dispatch to the `model_fit` methods directly
  expect_named(predict_numeric(lin_fit, hpc[1:5, ]), NULL)
  expect_length(predict_numeric(lin_fit, hpc[1:5, ]), 5L)
  expect_s3_class(predict_classprob(mn_fit, hpc[1:5, ]), "tbl_df")
  expect_length(predict_class(mn_fit, hpc[1:5, ]), 5L)

  # and give the same answers as `predict()`, which only differs by the
  # `.pred_` prefix that `format_predictions()` adds
  expect_equal(
    predict(lin_fit, hpc[1:5, ])$.pred,
    predict_numeric(lin_fit, hpc[1:5, ])
  )

  prob_direct <- predict_classprob(mn_fit, hpc[1:5, ])
  expect_named(prob_direct, levels(hpc$class))
  expect_equal(
    unname(as.matrix(prob_direct)),
    unname(as.matrix(predict(mn_fit, hpc[1:5, ], type = "prob")))
  )
})
