library(testthat)
context("simple neural network execution with nnet")
library(parsnip)

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
  
  skip_if_not_installed("nnet")
  
  xy_fit <- fit_xy(
    iris_nnet,
    x = iris[, num_pred],
    y = iris$Species,
    engine = "nnet",
    control = ctrl
  )

  xy_pred <- predict(xy_fit$fit, newdata = iris[1:8, num_pred], type = "class")
  xy_pred <- factor(xy_pred, levels = levels(iris$Species))
  expect_equal(xy_pred, predict_class(xy_fit, new_data = iris[1:8, num_pred]))

  form_fit <- fit(
    iris_nnet,
    Species ~ .,
    data = iris,
    engine = "nnet",
    control = ctrl
  )

  form_pred <- predict(form_fit$fit, newdata = iris[1:8, num_pred], type = "class")
  form_pred <- factor(form_pred, levels = levels(iris$Species))
  expect_equal(form_pred, predict_class(form_fit, new_data = iris[1:8, num_pred]))
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
  
  skip_if_not_installed("nnet")
  
  xy_fit <- fit_xy(
    car_basic,
    x = mtcars[, -1],
    y = mtcars$mpg,
    engine = "nnet",
    control = ctrl
  )

  xy_pred <- predict(xy_fit$fit, newdata = mtcars[1:8, -1])[,1]
  xy_pred <- unname(xy_pred)
  expect_equal(xy_pred, predict_num(xy_fit, new_data = mtcars[1:8, -1]))

  form_fit <- fit(
    car_basic,
    mpg ~ .,
    data = mtcars,
    engine = "nnet",
    control = ctrl
  )

  form_pred <- predict(form_fit$fit, newdata = mtcars[1:8, -1])[,1]
  form_pred <- unname(form_pred)
  expect_equal(form_pred, predict_num(form_fit, new_data = mtcars[1:8, -1]))
})

###################################################################

nn_dat <- read.csv("nnet_test.txt")

test_that('multivariate nnet formula', {
  
  skip_if_not_installed("nnet")
  
  nnet_form <- 
    mlp(
      mode = "regression",
      hidden_units = 3,
      penalty = 0.01
    ) %>% 
    parsnip::fit(
      cbind(V1, V2, V3) ~ ., 
      data = nn_dat[-(1:5),], 
      engine = "nnet"
    )
  expect_equal(length(nnet_form$fit$wts), 24)
  nnet_form_pred <- predict_num(nnet_form, new_data = nn_dat[1:5, -(1:3)])
  expect_equal(ncol(nnet_form_pred), 3)
  expect_equal(nrow(nnet_form_pred), 5)
  expect_equal(names(nnet_form_pred), c("V1", "V2", "V3")) 
  
  nnet_xy <- 
    mlp(
      mode = "regression",
      hidden_units = 3,
      penalty = 0.01
    ) %>% 
    parsnip::fit_xy(
      x = nn_dat[-(1:5), -(1:3)], 
      y = nn_dat[-(1:5),   1:3 ], 
      engine = "nnet"
    )
  expect_equal(length(nnet_xy$fit$wts), 24)
  nnet_form_xy <- predict_num(nnet_xy, new_data = nn_dat[1:5, -(1:3)])
  expect_equal(ncol(nnet_form_xy), 3)
  expect_equal(nrow(nnet_form_xy), 5)
  expect_equal(names(nnet_form_xy), c("V1", "V2", "V3")) 
})



