library(testthat)
context("logistic regression execution with glmnet")
library(parsnip)
library(rlang)
library(tibble)

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

  skip_if_not_installed("glmnet")

  expect_error(
    fit_xy(
      lc_basic,
      engine = "glmnet",
      control = ctrl,
      x = lending_club[, num_pred],
      y = lending_club$Class
    ),
    regexp = NA
  )

  glmnet_xy_catch <- fit_xy(
    lc_basic,
    x = lending_club[, num_pred],
    y = lending_club$total_bal_il,
    engine = "glmnet",
    control = caught_ctrl
  )
  expect_true(inherits(glmnet_xy_catch$fit, "try-error"))

})

test_that('glmnet prediction, one lambda', {

  skip_if_not_installed("glmnet")

  xy_fit <- fit_xy(
    logistic_reg(penalty = 0.1),
    engine = "glmnet",
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  uni_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            s = xy_fit$spec$args$penalty, type = "response")
  uni_pred <- predict(xy_fit$fit, newx = as.matrix(lending_club[1:7, num_pred]), type = "response")
  uni_pred <- ifelse(uni_pred >= 0.5, "good", "bad")
  uni_pred <- factor(uni_pred, levels = levels(lending_club$Class))
  uni_pred <- unname(uni_pred)

  expect_equal(uni_pred, predict_class(xy_fit, lending_club[1:7, num_pred]))

  res_form <- fit(
    logistic_reg(penalty = 0.1),
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
            s = res_form$spec$args$penalty)
  form_pred <- ifelse(form_pred >= 0.5, "good", "bad")
  form_pred <- factor(form_pred, levels = levels(lending_club$Class))
  form_pred <- unname(form_pred)
  expect_equal(form_pred, predict_class(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]))

})


test_that('glmnet prediction, mulitiple lambda', {

  skip_if_not_installed("glmnet")

  xy_fit <- fit_xy(
    logistic_reg(penalty = c(0.01, 0.1)),
    engine = "glmnet",
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  mult_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            s = xy_fit$spec$args$penalty, type = "response")
  mult_pred <- stack(as.data.frame(mult_pred))
  mult_pred$values <- ifelse(mult_pred$values >= 0.5, "good", "bad")
  mult_pred$values <- factor(mult_pred$values, levels = levels(lending_club$Class))
  mult_pred$lambda <- rep(xy_fit$spec$args$penalty, each = 7)
  mult_pred <- mult_pred[, -2]

  expect_equal(mult_pred, predict_class(xy_fit, lending_club[1:7, num_pred]))

  res_form <- fit(
    logistic_reg(penalty = c(0.01, 0.1)),
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
  form_pred$lambda <- rep(res_form$spec$args$penalty, each = 7)
  form_pred <- form_pred[, -2]
  expect_equal(form_pred, predict_class(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]))

})

test_that('glmnet prediction, no lambda', {

  skip_if_not_installed("glmnet")

  xy_fit <- fit_xy(
    logistic_reg(others = list(nlambda =  11)),
    engine = "glmnet",
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  mult_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            s = xy_fit$spec$args$penalty, type = "response")
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


test_that('glmnet probabilities, one lambda', {

  skip_if_not_installed("glmnet")

  xy_fit <- fit_xy(
    logistic_reg(penalty = 0.1),
    engine = "glmnet",
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  uni_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            s = xy_fit$spec$args$penalty, type = "response")[,1]
  uni_pred <- tibble(bad = 1 - uni_pred, good = uni_pred)

  expect_equal(uni_pred, predict_classprob(xy_fit, lending_club[1:7, num_pred]))

  res_form <- fit(
    logistic_reg(penalty = 0.1),
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
            s = res_form$spec$args$penalty, type = "response")[, 1]
  form_pred <- tibble(bad = 1 - form_pred, good = form_pred)
  expect_equal(form_pred, predict_classprob(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]))

  one_row <- predict_classprob(res_form, lending_club[1, c("funded_amnt", "int_rate")])
  expect_equal(form_pred[1,], one_row)

})

test_that('glmnet probabilities, mulitiple lambda', {

  skip_if_not_installed("glmnet")

  xy_fit <- fit_xy(
    logistic_reg(penalty = c(0.01, 0.1)),
    engine = "glmnet",
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  mult_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            s = xy_fit$spec$args$penalty, type = "response")
  mult_pred <- stack(as.data.frame(mult_pred))
  mult_pred <- tibble(bad = 1 - mult_pred$values, good = mult_pred$values)
  mult_pred$lambda <- rep(xy_fit$spec$args$penalty, each = 7)

  expect_equal(mult_pred, predict_classprob(xy_fit, lending_club[1:7, num_pred]))

  res_form <- fit(
    logistic_reg(penalty = c(0.01, 0.1)),
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
            s = res_form$spec$args$penalty, type = "response")
  form_pred <- stack(as.data.frame(form_pred))
  form_pred <- tibble(bad = 1 - form_pred$values, good = form_pred$values)
  form_pred$lambda <- rep(res_form$spec$args$penalty, each = 7)

  expect_equal(form_pred, predict_classprob(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]))

})


test_that('glmnet probabilities, no lambda', {

  skip_if_not_installed("glmnet")

  xy_fit <- fit_xy(
    logistic_reg(),
    engine = "glmnet",
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  mult_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            type = "response")
  mult_pred <- stack(as.data.frame(mult_pred))
  mult_pred <- tibble(bad = 1 - mult_pred$values, good = mult_pred$values)
  mult_pred$lambda <- rep(xy_fit$fit$lambda, each = 7)

  expect_equal(mult_pred, predict_classprob(xy_fit, lending_club[1:7, num_pred]))

  res_form <- fit(
    logistic_reg(),
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
  form_pred <- tibble(bad = 1 - form_pred$values, good = form_pred$values)
  form_pred$lambda <- rep(res_form$fit$lambda, each = 7)

  expect_equal(form_pred, predict_classprob(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]))

})


test_that('submodel prediction', {

  skip_if_not_installed("glmnet")

  vars <- c("female", "tenure", "total_charges", "phone_service", "monthly_charges")
  class_fit <-
    logistic_reg() %>%
    fit(churn ~ .,
        data = wa_churn[-(1:4), c("churn", vars)],
        engine = "glmnet")

  pred_glmn <- predict(class_fit$fit, as.matrix(wa_churn[1:4, vars]), s = .1, type = "response")

  mp_res <- multi_predict(class_fit, new_data = wa_churn[1:4, vars], penalty = .1, type = "prob")
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred_No"]], unname(pred_glmn[,1]))
})


