library(testthat)
library(parsnip)
library(rlang)
library(tidyr)

# ------------------------------------------------------------------------------

context("linear regression execution with glmnet")
source(test_path("helper-objects.R"))
hpc <- hpc_data[1:150, c(2:5, 8)]


num_pred <- c("compounds", "iterations", "num_pending")
hpc_bad_form <- as.formula(class ~ term)
hpc_basic <- linear_reg(penalty = .1, mixture = .3) %>%
  set_engine("glmnet", nlambda = 15)
no_lambda <- linear_reg(mixture = .3) %>%
  set_engine("glmnet")

# ------------------------------------------------------------------------------

test_that('glmnet execution', {

  skip_if_not_installed("glmnet")
  skip_if(run_glmnet)

  expect_error(
    res <- fit_xy(
      hpc_basic,
      control = ctrl,
      x = hpc[, num_pred],
      y = hpc$input_fields
    ),
    regexp = NA
  )

  expect_true(has_multi_predict(res))
  expect_equal(multi_predict_args(res), "penalty")

  expect_error(
    fit(
      hpc_basic,
      hpc_bad_form,
      data = hpc,
      control = ctrl
    )
  )

  glmnet_xy_catch <- fit_xy(
    hpc_basic,
    x = hpc[, num_pred],
    y = factor(hpc$input_fields),
    control = caught_ctrl
  )
  expect_true(inherits(glmnet_xy_catch$fit, "try-error"))

})

test_that('glmnet prediction, single lambda', {

  skip_if_not_installed("glmnet")
  skip_if(run_glmnet)

  res_xy <- fit_xy(
    hpc_basic,
    control = ctrl,
    x = hpc[, num_pred],
    y = hpc$input_fields
  )

  # glmn_mod <- glmnet::glmnet(x = as.matrix(hpc[, num_pred]), y = hpc$input_fields,
  #                            alpha = .3, nlambda = 15)

  uni_pred <- c(640.599944271351, 196.646976529848, 186.279646400216, 194.673852228774,
                198.126819755653)

  expect_equal(uni_pred, predict(res_xy, hpc[1:5, num_pred])$.pred, tolerance = 0.0001)

  res_form <- fit(
    hpc_basic,
    input_fields ~ log(compounds) + class,
    data = hpc,
    control = ctrl
  )

  form_pred <- c(570.504089227118, 162.413061474088, 167.022896537861, 157.609071878082,
                 165.887783741483)

  expect_equal(form_pred, predict(res_form, hpc[1:5,])$.pred, tolerance = 0.0001)
})


test_that('glmnet prediction, multiple lambda', {

  skip_if_not_installed("glmnet")
  skip_if(run_glmnet)

  lams <- c(.01, 0.1)

  hpc_mult <- linear_reg(penalty = lams, mixture = .3) %>%
    set_engine("glmnet")

  res_xy <- fit_xy(
    hpc_mult,
    control = ctrl,
    x = hpc[, num_pred],
    y = hpc$input_fields
  )

  # mult_pred <-
  #   predict(res_xy$fit,
  #           newx = as.matrix(hpc[1:5, num_pred]),
  #           s = lams)
  # mult_pred <- stack(as.data.frame(mult_pred))
  # mult_pred$penalty <- rep(lams, each = 5)
  # mult_pred$rows <- rep(1:5, 2)
  # mult_pred <- mult_pred[order(mult_pred$rows, mult_pred$penalty), ]
  # mult_pred <- mult_pred[, c("penalty", "values")]
  # names(mult_pred) <- c("penalty", ".pred")
  # mult_pred <- tibble::as_tibble(mult_pred)
  mult_pred <-
    tibble::tribble(
      ~penalty,           ~.pred,
      0.01, 639.672880668187,
      0.1, 639.672880668187,
      0.01, 197.744613311359,
      0.1, 197.744613311359,
      0.01, 187.737940787615,
      0.1, 187.737940787615,
      0.01, 195.780487678662,
      0.1, 195.780487678662,
      0.01, 199.217707535882,
      0.1, 199.217707535882
    )

  expect_equal(
    as.data.frame(mult_pred),
    multi_predict(res_xy, new_data = hpc[1:5, num_pred], lambda = lams) %>%
      unnest(cols = c(.pred)) %>%
      as.data.frame(),
    tolerance = 0.0001
  )

  res_form <- fit(
    hpc_mult,
    input_fields ~ log(compounds) + class,
    data = hpc,
    control = ctrl
  )

  # form_mat <- model.matrix(input_fields ~ log(compounds) + class, data = hpc)
  # form_mat <- form_mat[1:5, -1]
  #
  # form_pred <-
  #   predict(res_form$fit,
  #           newx = form_mat,
  #           s = lams)
  # form_pred <- stack(as.data.frame(form_pred))
  # form_pred$penalty <- rep(lams, each = 5)
  # form_pred$rows <- rep(1:5, 2)
  # form_pred <- form_pred[order(form_pred$rows, form_pred$penalty), ]
  # form_pred <- form_pred[, c("penalty", "values")]
  # names(form_pred) <- c("penalty", ".pred")
  # form_pred <- tibble::as_tibble(form_pred)

  form_pred <-
    tibble::tribble(
      ~penalty,           ~.pred,
      0.01, 570.474473760044,
      0.1, 570.474473760044,
      0.01, 164.040104978709,
      0.1, 164.040104978709,
      0.01, 168.709676954287,
      0.1, 168.709676954287,
      0.01, 159.173862504055,
      0.1, 159.173862504055,
      0.01, 167.559854709074,
      0.1, 167.559854709074
    )

  expect_equal(
    as.data.frame(form_pred),
    multi_predict(res_form, new_data = hpc[1:5, ], lambda = lams) %>%
      unnest(cols = c(.pred)) %>%
      as.data.frame(),
    tolerance = 0.0001
  )
})

test_that('glmnet prediction, all lambda', {

  skip_if_not_installed("glmnet")
  skip_if(run_glmnet)

  hpc_all <- linear_reg(mixture = .3) %>%
    set_engine("glmnet", nlambda = 7)

  res_xy <- fit_xy(
    hpc_all,
    control = ctrl,
    x = hpc[, num_pred],
    y = hpc$input_fields
  )

  all_pred <- predict(res_xy$fit, newx = as.matrix(hpc[1:5, num_pred]))
  all_pred <- stack(as.data.frame(all_pred))
  all_pred$penalty <- rep(res_xy$fit$lambda, each = 5)
  all_pred$rows <- rep(1:5, length(res_xy$fit$lambda))
  all_pred <- all_pred[order(all_pred$rows, all_pred$penalty), ]
  all_pred <- all_pred[, c("penalty", "values")]
  names(all_pred) <- c("penalty", ".pred")
  all_pred <- tibble::as_tibble(all_pred)

  expect_equal(all_pred, multi_predict(res_xy, new_data = hpc[1:5,num_pred ]) %>% unnest(cols = c(.pred)))

  res_form <- fit(
    hpc_all,
    input_fields ~ log(compounds) + class,
    data = hpc,
    control = ctrl
  )

  form_mat <- model.matrix(input_fields ~ log(compounds) + class, data = hpc)
  form_mat <- form_mat[1:5, -1]

  form_pred <- predict(res_form$fit, newx = form_mat)
  form_pred <- stack(as.data.frame(form_pred))
  form_pred$penalty <- rep(res_form$fit$lambda, each = 5)
  form_pred$rows <- rep(1:5, length(res_form$fit$lambda))
  form_pred <- form_pred[order(form_pred$rows, form_pred$penalty), ]
  form_pred <- form_pred[, c("penalty", "values")]
  names(form_pred) <- c("penalty", ".pred")
  form_pred <- tibble::as_tibble(form_pred)

  expect_equal(form_pred, multi_predict(res_form, hpc[1:5, c("compounds", "class")]) %>% unnest(cols = c(.pred)))
})


test_that('submodel prediction', {

  skip_if_not_installed("glmnet")
  skip_if(run_glmnet)

  reg_fit <-
    linear_reg() %>%
    set_engine("glmnet") %>%
    fit(mpg ~ ., data = mtcars[-(1:4), ])

  pred_glmn <- predict(reg_fit$fit, as.matrix(mtcars[1:4, -1]), s = .1)

  mp_res <- multi_predict(reg_fit, new_data = mtcars[1:4, -1], penalty = .1)
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred"]], unname(pred_glmn[,1]))

  expect_error(
    multi_predict(reg_fit, newdata = mtcars[1:4, -1], penalty = .1),
    "Did you mean"
  )

  reg_fit <-
    linear_reg() %>%
    set_engine("glmnet") %>%
    fit(mpg ~ ., data = mtcars[-(1:4), ])


  pred_glmn_all <-
    predict(reg_fit$fit, as.matrix(mtcars[1:2, -1])) %>%
    as.data.frame() %>%
    stack() %>%
    dplyr::arrange(ind)


  mp_res_all <-
    multi_predict(reg_fit, new_data = mtcars[1:2, -1]) %>%
    tidyr::unnest(cols = c(.pred))

  expect_equal(sort(mp_res_all$.pred), sort(pred_glmn_all$values))

})


test_that('error traps', {

  skip_if_not_installed("glmnet")
  skip_if(run_glmnet)

  expect_error(
    linear_reg() %>%
      set_engine("glmnet") %>%
      fit(mpg ~ ., data = mtcars[-(1:4), ]) %>%
      predict(mtcars[-(1:4), ], penalty = 0:1)
  )
  expect_error(
    linear_reg() %>%
      set_engine("glmnet") %>%
      fit(mpg ~ ., data = mtcars[-(1:4), ]) %>%
      predict(mtcars[-(1:4), ])
  )

})

