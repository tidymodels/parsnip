library(testthat)
context("simple neural network execution with keras")
library(parsnip)
library(keras)
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


test_that('keras classification prediction', {
  xy_fit <- parsnip::fit_xy(
    iris_keras,
    x = iris[, num_pred],
    y = iris$Species,
    engine = "keras",
    control = ctrl
  )
  
  xy_pred <- predict_classes(xy_fit$fit, x = as.matrix(iris[1:8, num_pred]))
  xy_pred <- factor(levels(iris$Species)[xy_pred + 1], levels = levels(iris$Species))
  expect_equal(xy_pred, predict_class(xy_fit, newdata = iris[1:8, num_pred]))
  
  keras::backend()$clear_session()
  
  form_fit <- parsnip::fit(
    iris_keras,
    Species ~ .,
    data = iris,
    engine = "keras",
    control = ctrl
  )
  
  form_pred <- predict_classes(form_fit$fit, x = as.matrix(iris[1:8, num_pred]))
  form_pred <- factor(levels(iris$Species)[form_pred + 1], levels = levels(iris$Species))
  expect_equal(form_pred, predict_class(form_fit, newdata = iris[1:8, num_pred]))
  
  keras::backend()$clear_session()
})


test_that('keras classification probabilities', {
  xy_fit <- parsnip::fit_xy(
    iris_keras,
    x = iris[, num_pred],
    y = iris$Species,
    engine = "keras",
    control = ctrl
  )
  
  xy_pred <- predict_proba(xy_fit$fit, x = as.matrix(iris[1:8, num_pred]))
  xy_pred <- as_tibble(xy_pred)
  colnames(xy_pred) <- levels(iris$Species)
  expect_equal(xy_pred, predict_classprob(xy_fit, newdata = iris[1:8, num_pred]))
  
  keras::backend()$clear_session()
  
  form_fit <- parsnip::fit(
    iris_keras,
    Species ~ .,
    data = iris,
    engine = "keras",
    control = ctrl
  )
  
  form_pred <- predict_proba(form_fit$fit, x = as.matrix(iris[1:8, num_pred]))
  form_pred <- as_tibble(form_pred)
  colnames(form_pred) <- levels(iris$Species)
  expect_equal(form_pred, predict_classprob(form_fit, newdata = iris[1:8, num_pred]))
  
  keras::backend()$clear_session()
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



test_that('keras regression prediction', {
  xy_fit <- parsnip::fit_xy(
    mlp(mode = "regression", hidden_units = 2, epochs = 500, regularization = .1),
    x = mtcars[, c("cyl", "disp")],
    y = mtcars$mpg,
    engine = "keras",
    control = ctrl
  )
  
  xy_pred <- predict(xy_fit$fit, x = as.matrix(mtcars[1:8, c("cyl", "disp")]))[,1]
  expect_equal(xy_pred, predict(xy_fit, newdata = mtcars[1:8, c("cyl", "disp")]))
  
  keras::backend()$clear_session()
  
  form_fit <- parsnip::fit(
    car_basic,
    mpg ~ .,
    data = mtcars[, c("cyl", "disp", "mpg")],,
    engine = "keras",
    control = ctrl
  )
  
  form_pred <- predict(form_fit$fit, x = as.matrix(mtcars[1:8, c("cyl", "disp")]))[,1]
  expect_equal(form_pred, predict(form_fit, newdata = mtcars[1:8, c("cyl", "disp")]))
  
  keras::backend()$clear_session()
})

