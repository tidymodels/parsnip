library(testthat)
context("boosted tree execution with xgboost")
library(parsnip)


###################################################################

num_pred <- names(iris)[1:4]

iris_xgboost <- boost_tree(trees = 2)

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


test_that('xgboost classification prediction', {

  skip_if_not_installed("xgboost")

  library(xgboost)
  xy_fit <- fit_xy(
    iris_xgboost,
    x = iris[, num_pred],
    y = iris$Species,
    engine = "xgboost",
    control = ctrl
  )

  xy_pred <- predict(xy_fit$fit, newdata = xgb.DMatrix(data = as.matrix(iris[1:8, num_pred])), type = "class")
  xy_pred <- matrix(xy_pred, ncol = 3, byrow = TRUE)
  xy_pred <- factor(levels(iris$Species)[apply(xy_pred, 1, which.max)], levels = levels(iris$Species))
  expect_equal(xy_pred, predict_class(xy_fit, new_data = iris[1:8, num_pred]))

  form_fit <- fit(
    iris_xgboost,
    Species ~ .,
    data = iris,
    engine = "xgboost",
    control = ctrl
  )

  form_pred <- predict(form_fit$fit, newdata = xgb.DMatrix(data = as.matrix(iris[1:8, num_pred])), type = "class")
  form_pred <- matrix(form_pred, ncol = 3, byrow = TRUE)
  form_pred <- factor(levels(iris$Species)[apply(form_pred, 1, which.max)], levels = levels(iris$Species))
  expect_equal(form_pred, predict_class(form_fit, new_data = iris[1:8, num_pred]))
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



test_that('xgboost regression prediction', {

  skip_if_not_installed("xgboost")

  xy_fit <- fit_xy(
    car_basic,
    x = mtcars[, -1],
    y = mtcars$mpg,
    engine = "xgboost",
    control = ctrl
  )

  xy_pred <- predict(xy_fit$fit, newdata = xgb.DMatrix(data = as.matrix(mtcars[1:8, -1])))
  expect_equal(xy_pred, predict_num(xy_fit, new_data = mtcars[1:8, -1]))

  form_fit <- fit(
    car_basic,
    mpg ~ .,
    data = mtcars,
    engine = "xgboost",
    control = ctrl
  )

  form_pred <- predict(form_fit$fit, newdata = xgb.DMatrix(data = as.matrix(mtcars[1:8, -1])))
  expect_equal(form_pred, predict_num(form_fit, new_data = mtcars[1:8, -1]))
})



test_that('submodel prediction', {

  skip_if_not_installed("xgboost")
  library(xgboost)

  reg_fit <-
    boost_tree(
      trees = 20,
      mode = "regression"
    ) %>%
    fit(mpg ~ ., data = mtcars[-(1:4), ], engine = "xgboost")

  x <-  xgboost::xgb.DMatrix(as.matrix(mtcars[1:4, -1]))

  pruned_pred <- predict(reg_fit$fit, x, ntreelimit = 5)

  mp_res <- multi_predict(reg_fit, new_data = mtcars[1:4, -1], trees = 5)
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred"]], pruned_pred)


  vars <- c("female", "tenure", "total_charges", "phone_service", "monthly_charges")
  class_fit <-
    boost_tree(trees = 20, mode = "classification") %>%
    fit(churn ~ .,
        data = wa_churn[-(1:4), c("churn", vars)],
        engine = "xgboost")

  x <-  xgboost::xgb.DMatrix(as.matrix(wa_churn[1:4, vars]))

  pred_class <- predict(class_fit$fit, x, ntreelimit = 5)

  mp_res <- multi_predict(class_fit, new_data = wa_churn[1:4, vars], trees = 5, type = "prob")
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred_No"]], pred_class)
})

