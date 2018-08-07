library(testthat)
context("simple neural network execution with nnet")
library(parsnip)
library(nnet)


###################################################################

num_pred <- names(iris)[1:4]

iris_nnet <- mlp(mode = "classification", hidden_units = 2)

ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)


test_that('nnet execution, classification', {

  skip_if_not_installed("nnet")

  expect_error(
    res <- parsnip::fit(
      iris_nnet,
      Species ~ Sepal.Width + Sepal.Length,
      data = iris,
      engine = "nnet",
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- parsnip::fit_xy(
      iris_nnet,
      x = iris[, num_pred],
      y = iris$Species,
      engine = "nnet",
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    res <- parsnip::fit(
      iris_nnet,
      Species ~ novar,
      data = iris,
      engine = "nnet",
      control = ctrl
    )
  )
})


test_that('nnet classification prediction', {
  xy_fit <- fit_xy(
    iris_nnet,
    x = iris[, num_pred],
    y = iris$Species,
    engine = "nnet",
    control = ctrl
  )

  xy_pred <- predict(xy_fit$fit, newdata = iris[1:8, num_pred], type = "class")
  xy_pred <- factor(xy_pred, levels = levels(iris$Species))
  expect_equal(xy_pred, predict_class(xy_fit, newdata = iris[1:8, num_pred]))

  form_fit <- fit(
    iris_nnet,
    Species ~ .,
    data = iris,
    engine = "nnet",
    control = ctrl
  )

  form_pred <- predict(form_fit$fit, newdata = iris[1:8, num_pred], type = "class")
  form_pred <- factor(form_pred, levels = levels(iris$Species))
  expect_equal(form_pred, predict_class(form_fit, newdata = iris[1:8, num_pred]))
})


###################################################################

num_pred <- names(mtcars)[3:6]

car_basic <- mlp(mode = "regression")

bad_nnet_reg <- mlp(mode = "regression",
                    others = list(min.node.size = -10))
bad_rf_reg <- mlp(mode = "regression",
                  others = list(sampsize = -10))

ctrl <- list(verbosity = 1, catch = FALSE)
caught_ctrl <- list(verbosity = 1, catch = TRUE)
quiet_ctrl <- list(verbosity = 0, catch = TRUE)

test_that('nnet execution, regression', {

  skip_if_not_installed("nnet")

  expect_error(
    res <- parsnip::fit(
      car_basic,
      mpg ~ .,
      data = mtcars,
      engine = "nnet",
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    res <- parsnip::fit_xy(
      car_basic,
      x = mtcars[, num_pred],
      y = mtcars$mpg,
      engine = "nnet",
      control = ctrl
    ),
    regexp = NA
  )
})



test_that('nnet regression prediction', {
  xy_fit <- fit_xy(
    car_basic,
    x = mtcars[, -1],
    y = mtcars$mpg,
    engine = "nnet",
    control = ctrl
  )

  xy_pred <- predict(xy_fit$fit, newdata = mtcars[1:8, -1])[,1]
  xy_pred <- unname(xy_pred)
  expect_equal(xy_pred, predict(xy_fit, newdata = mtcars[1:8, -1]))

  form_fit <- fit(
    car_basic,
    mpg ~ .,
    data = mtcars,
    engine = "nnet",
    control = ctrl
  )

  form_pred <- predict(form_fit$fit, newdata = mtcars[1:8, -1])[,1]
  form_pred <- unname(form_pred)
  expect_equal(form_pred, predict(form_fit, newdata = mtcars[1:8, -1]))
})

