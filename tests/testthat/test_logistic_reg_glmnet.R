library(testthat)
library(parsnip)
library(rlang)

data("lending_club")
lending_club <- head(lending_club, 200)
lc_form <- as.formula(Class ~ log(funded_amnt) + int_rate)
num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")
lc_bad_form <- as.formula(funded_amnt ~ term)
lc_basic <- logistic_reg()
ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)


test_that('glmnet execution', {
  skip_on_cran()

  expect_error(
    fit(
      lc_basic,
      engine = "glmnet",
      control = ctrl,
      x = lending_club[, num_pred],
      y = lending_club$Class
    ),
    regexp = NA
  )

  glmnet_xy_catch <- fit(
    lc_basic,
    x = lending_club[, num_pred],
    y = lending_club$total_bal_il,
    engine = "glmnet",
    control = caught_ctrl
  )
  expect_true(inherits(glmnet_xy_catch$fit, "try-error"))

})



test_that('glmnet prediction, one lambda', {
  skip_on_cran()

  xy_fit <- fit(
    logistic_reg(regularization = 0.1),
    engine = "glmnet",
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  uni_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            s = xy_fit$spec$args$regularization, type = "response")
  uni_pred <- predict(xy_fit$fit, newx = as.matrix(lending_club[1:7, num_pred]), type = "response")
  uni_pred <- ifelse(uni_pred >= 0.5, "good", "bad")
  uni_pred <- factor(uni_pred, levels = levels(lending_club$Class))
  uni_pred <- unname(uni_pred)

  expect_equal(uni_pred, predict_class(xy_fit, lending_club[1:7, num_pred]))

  res_form <- fit(
    logistic_reg(regularization = 0.1),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    engine = "glmnet",
    control = ctrl
  )

  form_mat <- model.matrix(Class ~ log(funded_amnt) + int_rate, data = lending_club)
  form_mat <- form_mat[1:7, -1]

  form_pred <-
    predict(res_form$fit,
            newx = form_mat,
            s = res_form$spec$args$regularization)
  form_pred <- ifelse(form_pred >= 0.5, "good", "bad")
  form_pred <- factor(form_pred, levels = levels(lending_club$Class))
  form_pred <- unname(form_pred)
  expect_equal(form_pred, predict_class(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]))

})


test_that('glmnet prediction, mulitiple lambda', {
  skip_on_cran()

  xy_fit <- fit(
    logistic_reg(regularization = c(0.01, 0.1)),
    engine = "glmnet",
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  mult_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            s = xy_fit$spec$args$regularization, type = "response")
  mult_pred <- stack(as.data.frame(mult_pred))
  mult_pred$values <- ifelse(mult_pred$values >= 0.5, "good", "bad")
  mult_pred$values <- factor(mult_pred$values, levels = levels(lending_club$Class))
  mult_pred$lambda <- rep(xy_fit$spec$args$regularization, each = 7)
  mult_pred <- mult_pred[, -2]

  expect_equal(mult_pred, predict_class(xy_fit, lending_club[1:7, num_pred]))

  res_form <- fit(
    logistic_reg(regularization = c(0.01, 0.1)),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    engine = "glmnet",
    control = ctrl
  )

  form_mat <- model.matrix(Class ~ log(funded_amnt) + int_rate, data = lending_club)
  form_mat <- form_mat[1:7, -1]

  form_pred <-
    predict(res_form$fit,
            newx = form_mat,
            type = "response")
  form_pred <- stack(as.data.frame(form_pred))
  form_pred$values <- ifelse(form_pred$values >= 0.5, "good", "bad")
  form_pred$values <- factor(form_pred$values, levels = levels(lending_club$Class))
  form_pred$lambda <- rep(res_form$spec$args$regularization, each = 7)
  form_pred <- form_pred[, -2]
  expect_equal(form_pred, predict_class(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]))

})

test_that('glmnet prediction, no lambda', {
  skip_on_cran()

  xy_fit <- fit(
    logistic_reg(others = list(nlambda =  11)),
    engine = "glmnet",
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  mult_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            s = xy_fit$spec$args$regularization, type = "response")
  mult_pred <- stack(as.data.frame(mult_pred))
  mult_pred$values <- ifelse(mult_pred$values >= 0.5, "good", "bad")
  mult_pred$values <- factor(mult_pred$values, levels = levels(lending_club$Class))
  mult_pred$lambda <- rep(xy_fit$fit$lambda, each = 7)
  mult_pred <- mult_pred[, -2]

  expect_equal(mult_pred, predict_class(xy_fit, lending_club[1:7, num_pred]))

  res_form <- fit(
    logistic_reg(others = list(nlambda =  11)),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    engine = "glmnet",
    control = ctrl
  )

  form_mat <- model.matrix(Class ~ log(funded_amnt) + int_rate, data = lending_club)
  form_mat <- form_mat[1:7, -1]

  form_pred <-
    predict(res_form$fit,
            newx = form_mat,
            type = "response")
  form_pred <- stack(as.data.frame(form_pred))
  form_pred$values <- ifelse(form_pred$values >= 0.5, "good", "bad")
  form_pred$values <- factor(form_pred$values, levels = levels(lending_club$Class))
  form_pred$lambda <- rep(res_form$fit$lambda, each = 7)
  form_pred <- form_pred[, -2]
  expect_equal(form_pred, predict_class(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]))

})
