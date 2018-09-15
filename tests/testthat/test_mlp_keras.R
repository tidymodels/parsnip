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


test_that('keras classification prediction', {
  
  skip_if_not_installed("keras")
  library(keras)
  
  xy_fit <- parsnip::fit_xy(
    iris_keras,
    x = iris[, num_pred],
    y = iris$Species,
    engine = "keras",
    control = ctrl
  )
  
  xy_pred <- predict_classes(xy_fit$fit, x = as.matrix(iris[1:8, num_pred]))
  xy_pred <- factor(levels(iris$Species)[xy_pred + 1], levels = levels(iris$Species))
  expect_equal(xy_pred, predict_class(xy_fit, new_data = iris[1:8, num_pred]))
  
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
  expect_equal(form_pred, predict_class(form_fit, new_data = iris[1:8, num_pred]))
  
  keras::backend()$clear_session()
})


test_that('keras classification probabilities', {
  
  skip_if_not_installed("keras")
  
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
  expect_equal(xy_pred, predict_classprob(xy_fit, new_data = iris[1:8, num_pred]))
  
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
  expect_equal(form_pred, predict_classprob(form_fit, new_data = iris[1:8, num_pred]))
  
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
  
  skip_if_not_installed("keras")
  
  xy_fit <- parsnip::fit_xy(
    mlp(mode = "regression", hidden_units = 2, epochs = 500, penalty = .1),
    x = mtcars[, c("cyl", "disp")],
    y = mtcars$mpg,
    engine = "keras",
    control = ctrl
  )
  
  xy_pred <- predict(xy_fit$fit, x = as.matrix(mtcars[1:8, c("cyl", "disp")]))[,1]
  expect_equal(xy_pred, predict_num(xy_fit, new_data = mtcars[1:8, c("cyl", "disp")]))
  
  keras::backend()$clear_session()
  
  form_fit <- parsnip::fit(
    car_basic,
    mpg ~ .,
    data = mtcars[, c("cyl", "disp", "mpg")],,
    engine = "keras",
    control = ctrl
  )
  
  form_pred <- predict(form_fit$fit, x = as.matrix(mtcars[1:8, c("cyl", "disp")]))[,1]
  expect_equal(form_pred, predict_num(form_fit, new_data = mtcars[1:8, c("cyl", "disp")]))
  
  keras::backend()$clear_session()
})


###################################################################

nn_dat <- read.csv("nnet_test.txt")

test_that('multivariate nnet formula', {
  
  skip_if_not_installed("keras")
  
  nnet_form <- 
    mlp(
      mode = "regression",
      hidden_units = 3,
      penalty = 0.01
    ) %>% 
    parsnip::fit(
      cbind(V1, V2, V3) ~ ., 
      data = nn_dat[-(1:5),], 
      engine = "keras"
    )
  expect_equal(length(unlist(keras::get_weights(nnet_form$fit))), 24)
  nnet_form_pred <- predict_num(nnet_form, new_data = nn_dat[1:5, -(1:3)])
  expect_equal(ncol(nnet_form_pred), 3)
  expect_equal(nrow(nnet_form_pred), 5)
  expect_equal(names(nnet_form_pred), c("V1", "V2", "V3")) 
  
  keras::backend()$clear_session()
  
  nnet_xy <- 
    mlp(
      mode = "regression",
      hidden_units = 3,
      penalty = 0.01
    ) %>% 
    parsnip::fit_xy(
      x = nn_dat[-(1:5), -(1:3)], 
      y = nn_dat[-(1:5),   1:3 ], 
      engine = "keras"
    )
  expect_equal(length(unlist(keras::get_weights(nnet_xy$fit))), 24)
  nnet_form_xy <- predict_num(nnet_xy, new_data = nn_dat[1:5, -(1:3)])
  expect_equal(ncol(nnet_form_xy), 3)
  expect_equal(nrow(nnet_form_xy), 5)
  expect_equal(names(nnet_form_xy), c("V1", "V2", "V3")) 
  
  keras::backend()$clear_session()
})
