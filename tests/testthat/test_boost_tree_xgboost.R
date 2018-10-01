library(testthat)
context("boosted tree execution with xgboost")
library(parsnip)


###################################################################

num_pred <- names(iris)[1:4]

iris_xgboost <- boost_tree(mode = "classification", trees = 2)

ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

test_that('xgboost execution, classification', {

  skip_if_not_installed("xgboost")

  expect_error(
    res <- parsnip::fit(
      iris_xgboost,
      Species ~ Sepal.Width + Sepal.Length,
      data = iris,
      engine = "xgboost",
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- parsnip::fit_xy(
      iris_xgboost,
      x = iris[, num_pred],
      y = iris$Species,
      engine = "xgboost",
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    res <- parsnip::fit(
      iris_xgboost,
      Species ~ novar,
      data = iris,
      engine = "xgboost",
      control = ctrl
    )
  )
})

###################################################################

num_pred <- names(mtcars)[3:6]

car_basic <- boost_tree(mode = "regression")

bad_xgboost_reg <- boost_tree(mode = "regression",
                              others = list(min.node.size = -10))
bad_rf_reg <- boost_tree(mode = "regression",
                         others = list(sampsize = -10))

ctrl <- list(verbosity = 1, catch = FALSE)
caught_ctrl <- list(verbosity = 1, catch = TRUE)
quiet_ctrl <- list(verbosity = 0, catch = TRUE)

test_that('xgboost execution, regression', {

  skip_if_not_installed("xgboost")

  expect_error(
    res <- parsnip::fit(
      car_basic,
      mpg ~ .,
      data = mtcars,
      engine = "xgboost",
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    res <- parsnip::fit_xy(
      car_basic,
      x = mtcars[, num_pred],
      y = mtcars$mpg,
      engine = "xgboost",
      control = ctrl
    ),
    regexp = NA
  )
})


