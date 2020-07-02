library(testthat)
library(parsnip)
library(rlang)
library(tibble)

# ------------------------------------------------------------------------------

context("multinom regression execution with glmnet")
source(test_path("helper-objects.R"))
hpc <- hpc_data[1:150, c(2:5, 8)]

rows <- c(1, 51, 101)

# ------------------------------------------------------------------------------

test_that('glmnet execution', {

  skip_if_not_installed("glmnet")
  skip_if(run_glmnet)

  expect_error(
    res <- fit_xy(
      multinom_reg() %>% set_engine("glmnet"),
      control = ctrl,
      x = hpc[, 1:4],
      y = hpc$class
    ),
    regexp = NA
  )

  expect_true(has_multi_predict(res))
  expect_equal(multi_predict_args(res), "penalty")

  expect_error(
    glmnet_xy_catch <- fit_xy(
      multinom_reg() %>% set_engine("glmnet"),
      x = hpc[, 2:5],
      y = hpc$compounds,
      control = caught_ctrl
    )
  )

})

test_that('glmnet prediction, one lambda', {

  skip_if_not_installed("glmnet")
  skip_if(run_glmnet)

  xy_fit <- fit_xy(
    multinom_reg(penalty = 0.1) %>% set_engine("glmnet"),
    control = ctrl,
    x = hpc[, 1:4],
    y = hpc$class
  )

  uni_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(hpc[rows, 1:4]),
            s = xy_fit$spec$args$penalty, type = "class")
  uni_pred <- factor(uni_pred[,1], levels = levels(hpc$class))
  uni_pred <- unname(uni_pred)

  expect_equal(uni_pred, predict(xy_fit, hpc[rows, 1:4], type = "class")$.pred_class)

  res_form <- fit(
    multinom_reg(penalty = 0.1) %>% set_engine("glmnet"),
    class ~ log(compounds) + input_fields,
    data = hpc,
    control = ctrl
  )

  form_mat <- model.matrix(class ~ log(compounds) + input_fields, data = hpc)
  form_mat <- form_mat[rows, -1]

  form_pred <-
    predict(res_form$fit,
            newx = form_mat,
            s = res_form$spec$args$penalty,
            type = "class")
  form_pred <- factor(form_pred[,1], levels = levels(hpc$class))
  expect_equal(form_pred, parsnip:::predict_class.model_fit(res_form, hpc[rows, c("compounds", "input_fields")]))
  expect_equal(form_pred, predict(res_form, hpc[rows, c("compounds", "input_fields")], type = "class")$.pred_class)

})


test_that('glmnet probabilities, mulitiple lambda', {

  skip_if_not_installed("glmnet")
  skip_if(run_glmnet)

  lams <- c(0.01, 0.1)

  xy_fit <- fit_xy(
    multinom_reg(penalty = lams) %>% set_engine("glmnet"),
    control = ctrl,
    x = hpc[, 1:4],
    y = hpc$class
  )

  expect_error(predict(xy_fit, hpc[rows, 1:4], type = "class"))
  expect_error(predict(xy_fit, hpc[rows, 1:4], type = "prob"))

  mult_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(hpc[rows, 1:4]),
            s = lams, type = "response")
  mult_pred <- apply(mult_pred, 3, as_tibble)
  mult_pred <- dplyr:::bind_rows(mult_pred)
  mult_probs <- mult_pred
  names(mult_pred) <- paste0(".pred_", names(mult_pred))
  mult_pred$penalty <- rep(lams, each = 3)
  mult_pred$row <- rep(1:3, 2)
  mult_pred <- mult_pred[order(mult_pred$row, mult_pred$penalty),]
  mult_pred <- split(mult_pred[, -5], mult_pred$row)
  names(mult_pred) <- NULL
  mult_pred <- tibble(.pred = mult_pred)

  expect_equal(
    mult_pred$.pred,
    multi_predict(xy_fit, hpc[rows, 1:4], penalty = lams, type = "prob")$.pred
  )

  mult_class <- factor(names(mult_probs)[apply(mult_probs, 1, which.max)],
                       levels = xy_fit$lvl)
  mult_class <- tibble(
    .pred_class = mult_class,
    penalty = rep(lams, each = 3),
    row = rep(1:3, 2)
  )
  mult_class <- mult_class[order(mult_class$row, mult_class$penalty),]
  mult_class <- split(mult_class[, -3], mult_class$row)
  names(mult_class) <- NULL
  mult_class <- tibble(.pred = mult_class)

  expect_equal(
    mult_class$.pred,
    multi_predict(xy_fit, hpc[rows, 1:4], penalty = lams)$.pred
  )

  expect_error(
    multi_predict(xy_fit, newdata = hpc[rows, 1:4], penalty = lams),
    "Did you mean"
  )

  # Can predict probs with default penalty. See #108
  expect_error(
    multi_predict(xy_fit, new_data = hpc[rows, 1:4], type = "prob"),
    NA
  )

})

test_that("class predictions are factors with all levels", {
  skip_if_not_installed("glmnet")
  skip_if(run_glmnet)

  basic <- multinom_reg() %>% set_engine("glmnet") %>% fit(class ~ ., data = hpc)
  nd <- hpc[hpc$class == "setosa", ]
  yhat <- predict(basic, new_data = nd, penalty = .1)
  yhat_multi <- multi_predict(basic, new_data =  nd, penalty = .1)$.pred
  expect_is(yhat_multi[[1]]$.pred_class, "factor")
  expect_equal(levels(yhat_multi[[1]]$.pred_class), levels(hpc$class))
})
