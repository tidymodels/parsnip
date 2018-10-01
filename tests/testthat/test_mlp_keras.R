library(testthat)
context("simple neural network execution with keras")
library(parsnip)
library(tibble)

###################################################################

num_pred <- names(iris)[1:4]

iris_keras <- mlp(mode = "classification", hidden_units = 2)

ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

test_that('keras execution, classification', {

  skip_if_not_installed("keras")

  expect_error(
    res <- parsnip::fit(
      iris_keras,
      Species ~ Sepal.Width + Sepal.Length,
      data = iris,
      engine = "keras",
      control = ctrl
    ),
    regexp = NA
  )

  keras::backend()$clear_session()

  expect_error(
    res <- parsnip::fit_xy(
      iris_keras,
      x = iris[, num_pred],
      y = iris$Species,
      engine = "keras",
      control = ctrl
    ),
    regexp = NA
  )

  keras::backend()$clear_session()

  expect_error(
    res <- parsnip::fit(
      iris_keras,
      Species ~ novar,
      data = iris,
      engine = "keras",
      control = ctrl
    )
  )
})



###################################################################

mtcars <- as.data.frame(scale(mtcars))

num_pred <- names(mtcars)[3:6]

car_basic <- mlp(mode = "regression")

bad_keras_reg <- mlp(mode = "regression",
                    others = list(min.node.size = -10))
bad_rf_reg <- mlp(mode = "regression",
                  others = list(sampsize = -10))

ctrl <- list(verbosity = 1, catch = FALSE)
caught_ctrl <- list(verbosity = 1, catch = TRUE)
quiet_ctrl <- list(verbosity = 0, catch = TRUE)

test_that('keras execution, regression', {

  skip_if_not_installed("keras")

  expect_error(
    res <- parsnip::fit(
      car_basic,
      mpg ~ .,
      data = mtcars,
      engine = "keras",
      control = ctrl
    ),
    regexp = NA
  )

  keras::backend()$clear_session()

  expect_error(
    res <- parsnip::fit_xy(
      car_basic,
      x = mtcars[, num_pred],
      y = mtcars$mpg,
      engine = "keras",
      control = ctrl
    ),
    regexp = NA
  )
})


