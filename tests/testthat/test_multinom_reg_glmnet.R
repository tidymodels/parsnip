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
    fit_xy(
      multinom_reg(),
      engine = "glmnet",
      control = ctrl,
      x = iris[, 1:4],
      y = iris$Species
    ),
    regexp = NA
  )

  glmnet_xy_catch <- fit_xy(
    multinom_reg(),
    x = iris[, 2:5],
    y = iris$Sepal.Length,
    engine = "glmnet",
    control = caught_ctrl
  )
  expect_true(inherits(glmnet_xy_catch$fit, "try-error"))

})

test_that('glmnet prediction, one lambda', {

  skip_if_not_installed("glmnet")

  xy_fit <- fit_xy(
    multinom_reg(penalty = 0.1),
    engine = "glmnet",
    control = ctrl,
    x = iris[, 1:4],
    y = iris$Species
  )

  uni_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(iris[rows, 1:4]),
            s = xy_fit$spec$args$penalty, type = "class")
  uni_pred <- factor(uni_pred[,1], levels = levels(iris$Species))
  uni_pred <- unname(uni_pred)

  expect_equal(uni_pred, predict_class(xy_fit, iris[rows, 1:4]))
  expect_equal(uni_pred, predict(xy_fit, iris[rows, 1:4], type = "class")$.pred_class)

  res_form <- fit(
    multinom_reg(penalty = 0.1),
    Species ~ log(Sepal.Width) + Petal.Width,
    data = iris,
    engine = "glmnet",
    control = ctrl
  )

  form_mat <- model.matrix(Species ~ log(Sepal.Width) + Petal.Width, data = iris)
  form_mat <- form_mat[rows, -1]

  form_pred <-
    predict(res_form$fit,
            newx = form_mat,
            s = res_form$spec$args$penalty,
            type = "class")
  form_pred <- factor(form_pred[,1], levels = levels(iris$Species))
  expect_equal(form_pred, predict_class(res_form, iris[rows, c("Sepal.Width", "Petal.Width")]))
  expect_equal(form_pred, predict(res_form, iris[rows, c("Sepal.Width", "Petal.Width")], type = "class")$.pred_class)

})


test_that('glmnet probabilities, mulitiple lambda', {

  skip_if_not_installed("glmnet")

  xy_fit <- fit_xy(
    multinom_reg(penalty = c(0.01, 0.1)),
    engine = "glmnet",
    control = ctrl,
    x = iris[, 1:4],
    y = iris$Species
  )

  expect_error(predict(xy_fit, iris[rows, 1:4], type = "class"))
  expect_error(predict(xy_fit, iris[rows, 1:4], type = "prob"))

  mult_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(iris[rows, 1:4]),
            s = xy_fit$spec$args$penalty, type = "response")
  mult_pred <- apply(mult_pred, 3, as_tibble)
  mult_pred <- dplyr:::bind_rows(mult_pred)
  mult_probs <- mult_pred
  names(mult_pred) <- paste0(".pred_", names(mult_pred))
  mult_pred$penalty <- rep(xy_fit$spec$args$penalty, each = 3)
  mult_pred$row <- rep(1:3, 2)
  mult_pred <- mult_pred[order(mult_pred$row, mult_pred$penalty),]
  mult_pred <- split(mult_pred[, -5], mult_pred$row)
  names(mult_pred) <- NULL
  mult_pred <- tibble(.pred = mult_pred)

  expect_equal(
    mult_pred$.pred,
    multi_predict(xy_fit, iris[rows, 1:4], penalty = xy_fit$spec$args$penalty, type = "prob")$.pred
  )

  mult_class <- names(mult_probs)[apply(mult_probs, 1, which.max)]
  mult_class <- tibble(
    .pred = mult_class,
    penalty = rep(xy_fit$spec$args$penalty, each = 3),
    row = rep(1:3, 2)
  )
  mult_class <- mult_class[order(mult_class$row, mult_class$penalty),]
  mult_class <- split(mult_class[, -3], mult_class$row)
  names(mult_class) <- NULL
  mult_class <- tibble(.pred = mult_class)

  expect_equal(
    mult_class$.pred,
    multi_predict(xy_fit, iris[rows, 1:4], penalty = xy_fit$spec$args$penalty)$.pred
  )
})


