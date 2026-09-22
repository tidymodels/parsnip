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
