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

  uni_pred <- c(5.05125589060219, 4.86977761622526, 4.90912345599309, 4.93931874108359,
                5.08755154547758)

  expect_equal(uni_pred, predict(res_xy, hpc[1:5, num_pred])$.pred, tolerance = 0.0001)

  res_form <- fit(
    hpc_basic,
    input_fields ~ log(compounds) + class,
    data = hpc,
    control = ctrl
  )

  form_pred <- c(5.23960117346944, 5.08769210344022, 5.15129212608077, 5.12000510716518,
                 5.26736239856889)

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
      0.01, 5.01352459498158,
      0.1, 5.05124049139868,
      0.01, 4.71767499960808,
      0.1, 4.87103404621362,
      0.01,  4.7791916685127,
      0.1, 4.91028250633598,
      0.01, 4.83366808792755,
      0.1,  4.9399094532023,
      0.01, 5.07269451405628,
      0.1, 5.08728178043569
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
      0.01, 5.09237402805557,
      0.1, 5.24228948237804,
      0.01, 4.75071416991856,
      0.1, 5.09448280355765,
      0.01, 4.89375747015535,
      0.1, 5.15636527125752,
      0.01, 4.82338959520112,
      0.1, 5.12592317615935,
      0.01, 5.15481201301174,
      0.1, 5.26930099973607
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
    set_engine("glmnet")

  res_xy <- fit_xy(
    hpc_all,
    control = ctrl,
    x = hpc[, num_pred],
    y = hpc$input_fields
  )

  all_pred <- predict(res_xy$fit, newx = as.matrix(hpc[1:5, num_pred]))
  all_pred <- stack(as.data.frame(all_pred))
  all_pred$penalty <- rep(res_xy$fit$lambda, each = 5)
  all_pred$rows <- rep(1:5, 2)
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
  form_pred$rows <- rep(1:5, 2)
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

