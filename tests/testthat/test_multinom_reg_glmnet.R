library(testthat)
context("multinom regression execution with glmnet")
library(parsnip)
library(rlang)
library(tibble)

ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

rows <- c(1, 51, 101)

test_that('glmnet execution', {

  skip_if_not_installed("glmnet")

  expect_error(
    res <- fit_xy(
      multinom_reg(),
      engine = "glmnet",
      control = ctrl,
      x = iris[, 1:4],
      y = iris$Species
    ),
    regexp = NA
  )

  expect_silent(pred <- predict(res, iris[, 1:4], penalty = 1))
  check_predict_basic(pred, iris)

  glmnet_xy_catch <- fit_xy(
    multinom_reg(),
    x = iris[, 2:5],
    y = iris$Sepal.Length,
    engine = "glmnet",
    control = caught_ctrl
  )
  expect_true(inherits(glmnet_xy_catch$fit, "try-error"))

})



