library(testthat)
library(parsnip)
library(rlang)
library(tibble)
library(tidyr)

# ------------------------------------------------------------------------------

context("logistic regression execution with glmnet")

data("lending_club")
lending_club <- head(lending_club, 200)
lc_form <- as.formula(Class ~ log(funded_amnt) + int_rate)
num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")
lc_bad_form <- as.formula(funded_amnt ~ term)
lc_basic <- logistic_reg() %>% set_engine("glmnet")

ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

# ------------------------------------------------------------------------------

test_that('glmnet execution', {

  skip_if_not_installed("glmnet")

  expect_error(
    fit_xy(
      lc_basic,
      control = ctrl,
      x = lending_club[, num_pred],
      y = lending_club$Class
    ),
    regexp = NA
  )

  expect_error(
    glmnet_xy_catch <- fit_xy(
      lc_basic,
      x = lending_club[, num_pred],
      y = lending_club$total_bal_il,
      control = caught_ctrl
    )
  )
})

test_that('glmnet prediction, one lambda', {

  skip_if_not_installed("glmnet")

  xy_fit <- fit_xy(
    logistic_reg(penalty = 0.1) %>% set_engine("glmnet"),
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  uni_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            s = 0.1, type = "response")
  uni_pred <- predict(xy_fit$fit, newx = as.matrix(lending_club[1:7, num_pred]), type = "response")
  uni_pred <- ifelse(uni_pred >= 0.5, "good", "bad")
  uni_pred <- factor(uni_pred, levels = levels(lending_club$Class))
  uni_pred <- unname(uni_pred)

  expect_equal(uni_pred, predict(xy_fit, lending_club[1:7, num_pred])$.pred_class)

  res_form <- fit(
    logistic_reg(penalty = 0.1) %>% set_engine("glmnet"),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    control = ctrl
  )

  form_mat <- model.matrix(Class ~ log(funded_amnt) + int_rate, data = lending_club)
  form_mat <- form_mat[1:7, -1]

  form_pred <-
    predict(res_form$fit,
            newx = form_mat,
            s = 0.1)
  form_pred <- ifelse(form_pred >= 0.5, "good", "bad")
  form_pred <- factor(form_pred, levels = levels(lending_club$Class))
  form_pred <- unname(form_pred)

  expect_equal(
    form_pred,
    predict(res_form, lending_club[1:7, c("funded_amnt", "int_rate")], type = "class")$.pred_class
  )

})


test_that('glmnet prediction, mulitiple lambda', {

  skip_if_not_installed("glmnet")

  lams <- c(0.01, 0.1)

  xy_fit <- fit_xy(
    logistic_reg(penalty = lams) %>% set_engine("glmnet"),
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  mult_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            s = lams, type = "response")
  mult_pred <- stack(as.data.frame(mult_pred))
  mult_pred$values <- ifelse(mult_pred$values >= 0.5, "good", "bad")
  mult_pred$values <- factor(mult_pred$values, levels = levels(lending_club$Class))
  mult_pred$penalty <- rep(lams, each = 7)
  mult_pred$rows <- rep(1:7, 2)
  mult_pred <- mult_pred[order(mult_pred$rows, mult_pred$penalty), ]
  mult_pred <- mult_pred[, c("penalty", "values")]
  names(mult_pred) <- c("penalty", ".pred")
  mult_pred <- tibble::as_tibble(mult_pred)

  expect_equal(
    mult_pred,
    multi_predict(xy_fit, lending_club[1:7, num_pred], type = "class") %>% unnest()
    )

  res_form <- fit(
    logistic_reg(penalty = lams) %>% set_engine("glmnet"),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    control = ctrl
  )

  form_mat <- model.matrix(Class ~ log(funded_amnt) + int_rate, data = lending_club)
  form_mat <- form_mat[1:7, -1]

  form_pred <-
    predict(res_form$fit,
            newx = form_mat,
            s = lams)
  form_pred <- stack(as.data.frame(form_pred))
  form_pred$values <- ifelse(form_pred$values >= 0.5, "good", "bad")
  form_pred$values <- factor(form_pred$values, levels = levels(lending_club$Class))
  form_pred$penalty <- rep(lams, each = 7)
  form_pred$rows <- rep(1:7, 2)
  form_pred <- form_pred[order(form_pred$rows, form_pred$penalty), ]
  form_pred <- form_pred[, c("penalty", "values")]
  names(form_pred) <- c("penalty", ".pred")
  form_pred <- tibble::as_tibble(form_pred)

  expect_equal(
    form_pred,
    multi_predict(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]) %>% unnest()
  )

})

test_that('glmnet prediction, no lambda', {

  skip_if_not_installed("glmnet")

  xy_fit <- fit_xy(
    logistic_reg() %>% set_engine("glmnet", nlambda =  11),
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  mult_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            s = xy_fit$fit$lambda, type = "response")
  mult_pred <- stack(as.data.frame(mult_pred))
  mult_pred$values <- ifelse(mult_pred$values >= 0.5, "good", "bad")
  mult_pred$values <- factor(mult_pred$values, levels = levels(lending_club$Class))
  mult_pred$penalty <- rep(xy_fit$fit$lambda, each = 7)
  mult_pred$rows <- rep(1:7, 2)
  mult_pred <- mult_pred[order(mult_pred$rows, mult_pred$penalty), ]
  mult_pred <- mult_pred[, c("penalty", "values")]
  names(mult_pred) <- c("penalty", ".pred")
  mult_pred <- tibble::as_tibble(mult_pred)

  expect_equal(mult_pred, multi_predict(xy_fit, lending_club[1:7, num_pred]) %>% unnest())

  res_form <- fit(
    logistic_reg() %>% set_engine("glmnet", nlambda =  11),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
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
  form_pred$penalty <- rep(res_form$fit$lambda, each = 7)
  form_pred$rows <- rep(1:7, 2)
  form_pred <- form_pred[order(form_pred$rows, form_pred$penalty), ]
  form_pred <- form_pred[, c("penalty", "values")]
  names(form_pred) <- c("penalty", ".pred")
  form_pred <- tibble::as_tibble(form_pred)

  expect_equal(
    form_pred,
    multi_predict(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]) %>% unnest()
  )

})


test_that('glmnet probabilities, one lambda', {

  skip_if_not_installed("glmnet")

  xy_fit <- fit_xy(
    logistic_reg(penalty = 0.1)  %>% set_engine("glmnet"),
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  uni_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            s = 0.1, type = "response")[,1]
  uni_pred <- tibble(.pred_bad = 1 - uni_pred, .pred_good = uni_pred)

  expect_equal(
    uni_pred,
    predict(xy_fit, lending_club[1:7, num_pred], type = "prob")
    )

  res_form <- fit(
    logistic_reg(penalty = 0.1)  %>% set_engine("glmnet"),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    control = ctrl
  )

  form_mat <- model.matrix(Class ~ log(funded_amnt) + int_rate, data = lending_club)
  form_mat <- form_mat[1:7, -1]

  form_pred <-
    predict(res_form$fit,
            newx = form_mat,
            s = 0.1, type = "response")[, 1]
  form_pred <- tibble(.pred_bad = 1 - form_pred, .pred_good = form_pred)

  expect_equal(
    form_pred,
    predict(res_form, lending_club[1:7, c("funded_amnt", "int_rate")], type = "prob")
    )

  one_row <- predict(res_form, lending_club[1, c("funded_amnt", "int_rate")], type = "prob")
  expect_equal(form_pred[1,], one_row)

})

test_that('glmnet probabilities, mulitiple lambda', {

  skip_if_not_installed("glmnet")

  lams <- c(0.01, 0.1)

  xy_fit <- fit_xy(
    logistic_reg(penalty = lams)  %>% set_engine("glmnet"),
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  mult_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            s = lams, type = "response")
  mult_pred <- stack(as.data.frame(mult_pred))
  mult_pred$penalty <- rep(lams, each = 7)
  mult_pred$rows <- rep(1:7, 2)
  mult_pred <- mult_pred[order(mult_pred$rows, mult_pred$penalty), ]
  mult_pred$.pred_bad <- 1 - mult_pred$values
  mult_pred <- mult_pred[, c("penalty", ".pred_bad", "values")]
  names(mult_pred) <- c("penalty", ".pred_bad", ".pred_good")
  mult_pred <- tibble::as_tibble(mult_pred)

  expect_equal(
    mult_pred,
    multi_predict(xy_fit, lending_club[1:7, num_pred], lambda = lams, type = "prob") %>%
      unnest()
    )

  res_form <- fit(
    logistic_reg(penalty = lams)  %>% set_engine("glmnet"),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    control = ctrl
  )

  form_mat <- model.matrix(Class ~ log(funded_amnt) + int_rate, data = lending_club)
  form_mat <- form_mat[1:7, -1]

  form_pred <-
    predict(res_form$fit,
            newx = form_mat,
            s = lams, type = "response")
  form_pred <- stack(as.data.frame(form_pred))
  form_pred$penalty <- rep(lams, each = 7)
  form_pred$rows <- rep(1:7, 2)
  form_pred <- form_pred[order(form_pred$rows, form_pred$penalty), ]
  form_pred$.pred_bad <- 1 - form_pred$values
  form_pred <- form_pred[, c("penalty", ".pred_bad", "values")]
  names(form_pred) <- c("penalty", ".pred_bad", ".pred_good")
  form_pred <- tibble::as_tibble(form_pred)

  expect_equal(
    form_pred,
    multi_predict(res_form, lending_club[1:7, c("funded_amnt", "int_rate")], type = "prob") %>%
      unnest()
    )

})


test_that('glmnet probabilities, no lambda', {

  skip_if_not_installed("glmnet")

  xy_fit <- fit_xy(
    logistic_reg()  %>% set_engine("glmnet"),
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  mult_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            type = "response")
  mult_pred <- stack(as.data.frame(mult_pred))
  mult_pred$penalty <- rep(xy_fit$fit$lambda, each = 7)
  mult_pred$rows <- rep(1:7, length(xy_fit$fit$lambda))
  mult_pred <- mult_pred[order(mult_pred$rows, mult_pred$penalty), ]
  mult_pred$.pred_bad <- 1 - mult_pred$values
  mult_pred <- mult_pred[, c("penalty", ".pred_bad", "values")]
  names(mult_pred) <- c("penalty", ".pred_bad", ".pred_good")
  mult_pred <- tibble::as_tibble(mult_pred)

  expect_equal(
    mult_pred,
    multi_predict(xy_fit, lending_club[1:7, num_pred], type = "prob") %>% unnest()
    )

  res_form <- fit(
    logistic_reg() %>% set_engine("glmnet"),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    control = ctrl
  )

  form_mat <- model.matrix(Class ~ log(funded_amnt) + int_rate, data = lending_club)
  form_mat <- form_mat[1:7, -1]

  form_pred <-
    predict(res_form$fit,
            newx = form_mat,
            type = "response")
  form_pred <- stack(as.data.frame(form_pred))
  form_pred$penalty <- rep(res_form$fit$lambda, each = 7)
  form_pred$rows <- rep(1:7, length(res_form$fit$lambda))
  form_pred <- form_pred[order(form_pred$rows, form_pred$penalty), ]
  form_pred$.pred_bad <- 1 - form_pred$values
  form_pred <- form_pred[, c("penalty", ".pred_bad", "values")]
  names(form_pred) <- c("penalty", ".pred_bad", ".pred_good")
  form_pred <- tibble::as_tibble(form_pred)

  expect_equal(
    form_pred,
    multi_predict(res_form, lending_club[1:7, c("funded_amnt", "int_rate")], type = "prob") %>% unnest()
  )

})


test_that('submodel prediction', {

  skip_if_not_installed("glmnet")

  vars <- c("female", "tenure", "total_charges", "phone_service", "monthly_charges")
  class_fit <-
    logistic_reg() %>%
    set_engine("glmnet") %>%
    fit(churn ~ ., data = wa_churn[-(1:4), c("churn", vars)])

  pred_glmn <- predict(class_fit$fit, as.matrix(wa_churn[1:4, vars]), s = .1, type = "response")

  mp_res <- multi_predict(class_fit, new_data = wa_churn[1:4, vars], penalty = .1, type = "prob")
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred_No"]], unname(pred_glmn[,1]))

  expect_error(
    multi_predict(class_fit, newdata = wa_churn[1:4, vars], penalty = .1, type = "prob"),
    "Did you mean"
  )

  # Can predict using default penalty. See #108
  expect_error(
    multi_predict(class_fit, new_data = wa_churn[1:4, vars]),
    NA
  )

})


