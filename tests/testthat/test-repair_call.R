test_that("repair_call errors for non-model_fit objects", {
  # Test with a list (not a model_fit)
  expect_snapshot(
    error = TRUE,
    repair_call(list(a = 1), mtcars)
  )

  # Test with a data frame
  expect_snapshot(
    error = TRUE,
    repair_call(mtcars, mtcars)
  )
})

test_that("repair_call works with fitted parsnip models", {
  skip_if_not_installed("modeldata")

  fitted_model <-
    linear_reg() |>
    set_engine("lm") |>
    fit(mpg ~ ., data = mtcars)

  repaired <- repair_call(fitted_model, mtcars)

  expect_s3_class(repaired, "model_fit")
  expect_equal(repaired$fit$call$data, quote(mtcars))
})
