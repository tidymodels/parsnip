library(testthat)
library(parsnip)

# ------------------------------------------------------------------------------

context("boosted tree execution with xgboost")
source(test_path("helper-objects.R"))
hpc <- hpc_data[1:150, c(2:5, 8)]

num_pred <- names(hpc)[1:4]

hpc_xgboost <-
  boost_tree(trees = 2, mode = "classification") %>%
  set_engine("xgboost")

# ------------------------------------------------------------------------------

test_that('xgboost execution, classification', {

  skip_if_not_installed("xgboost")

  expect_error(
    res <- parsnip::fit(
      hpc_xgboost,
      class ~ compounds + input_fields,
      data = hpc,
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- parsnip::fit_xy(
      hpc_xgboost,
      x = hpc[, num_pred],
      y = hpc$class,
      control = ctrl
    ),
    regexp = NA
  )

  expect_true(has_multi_predict(res))
  expect_equal(multi_predict_args(res), "trees")

  expect_error(
    res <- parsnip::fit(
      hpc_xgboost,
      class ~ novar,
      data = hpc,
      control = ctrl
    )
  )
})


test_that('xgboost classification prediction', {

  skip_if_not_installed("xgboost")

  library(xgboost)
  xy_fit <- fit_xy(
    hpc_xgboost,
    x = hpc[, num_pred],
    y = hpc$class,
    control = ctrl
  )

  xy_pred <- predict(xy_fit$fit, newdata = xgb.DMatrix(data = as.matrix(hpc[1:8, num_pred])), type = "class")
  xy_pred <- matrix(xy_pred, ncol = 4, byrow = TRUE)
  xy_pred <- factor(levels(hpc$class)[apply(xy_pred, 1, which.max)], levels = levels(hpc$class))
  expect_equal(xy_pred, predict(xy_fit, new_data = hpc[1:8, num_pred], type = "class")$.pred_class)

  form_fit <- fit(
    hpc_xgboost,
    class ~ .,
    data = hpc,
    control = ctrl
  )

  form_pred <- predict(form_fit$fit, newdata = xgb.DMatrix(data = as.matrix(hpc[1:8, num_pred])), type = "class")
  form_pred <- matrix(form_pred, ncol = 4, byrow = TRUE)
  form_pred <- factor(levels(hpc$class)[apply(form_pred, 1, which.max)], levels = levels(hpc$class))
  expect_equal(form_pred, predict(form_fit, new_data = hpc[1:8, num_pred], type = "class")$.pred_class)
})


# ------------------------------------------------------------------------------

num_pred <- names(mtcars)[3:6]

car_basic <-
  boost_tree(mode = "regression") %>%
  set_engine("xgboost")

bad_xgboost_reg <-
  boost_tree(mode = "regression") %>%
  set_engine("xgboost", min.node.size = -10)

bad_rf_reg <-
  boost_tree(mode = "regression") %>%
  set_engine("xgboost", sampsize = -10)

test_that('xgboost execution, regression', {

  skip_if_not_installed("xgboost")

  expect_error(
    res <- parsnip::fit(
      car_basic,
      mpg ~ .,
      data = mtcars,
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    res <- parsnip::fit_xy(
      car_basic,
      x = mtcars[, num_pred],
      y = mtcars$mpg,
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
    control = ctrl
  )

  xy_pred <- predict(xy_fit$fit, newdata = xgb.DMatrix(data = as.matrix(mtcars[1:8, -1])))
  expect_equal(xy_pred, predict(xy_fit, new_data = mtcars[1:8, -1])$.pred)

  form_fit <- fit(
    car_basic,
    mpg ~ .,
    data = mtcars,
    control = ctrl
  )

  form_pred <- predict(form_fit$fit, newdata = xgb.DMatrix(data = as.matrix(mtcars[1:8, -1])))
  expect_equal(form_pred, predict(form_fit, new_data = mtcars[1:8, -1])$.pred)
})



test_that('submodel prediction', {

  skip_if_not_installed("xgboost")
  library(xgboost)

  reg_fit <-
    boost_tree(trees = 20, mode = "regression") %>%
    set_engine("xgboost") %>%
    fit(mpg ~ ., data = mtcars[-(1:4), ])

  x <-  xgboost::xgb.DMatrix(as.matrix(mtcars[1:4, -1]))

  pruned_pred <- predict(reg_fit$fit, x, ntreelimit = 5)

  mp_res <- multi_predict(reg_fit, new_data = mtcars[1:4, -1], trees = 5)
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred"]], pruned_pred)


  vars <- c("female", "tenure", "total_charges", "phone_service", "monthly_charges")
  class_fit <-
    boost_tree(trees = 20, mode = "classification") %>%
    set_engine("xgboost") %>%
    fit(churn ~ ., data = wa_churn[-(1:4), c("churn", vars)])

  x <-  xgboost::xgb.DMatrix(as.matrix(wa_churn[1:4, vars]))

  pred_class <- predict(class_fit$fit, x, ntreelimit = 5)

  mp_res <- multi_predict(class_fit, new_data = wa_churn[1:4, vars], trees = 5, type = "prob")
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred_No"]], pred_class)

  expect_error(
    multi_predict(class_fit, newdata = wa_churn[1:4, vars], trees = 5, type = "prob"),
    "Did you mean"
  )
})


test_that('default engine', {
  skip_if_not_installed("xgboost")
  expect_warning(
    fit <- boost_tree(mode = "regression") %>% fit(mpg ~ ., data = mtcars),
    "Engine set to"
  )
  expect_true(inherits(fit$fit, "xgb.Booster"))
})

# ------------------------------------------------------------------------------

test_that('validation sets', {
  skip_if_not_installed("xgboost")
  expect_error(
    reg_fit <-
      boost_tree(trees = 20, mode = "regression") %>%
      set_engine("xgboost", validation = .1) %>%
      fit(mpg ~ ., data = mtcars[-(1:4), ]),
    regex = NA
  )

  expect_equal(colnames(reg_fit$fit$evaluation_log)[2], "validation_rmse")

  expect_error(
    reg_fit <-
      boost_tree(trees = 20, mode = "regression") %>%
      set_engine("xgboost", validation = .1, eval_metric = "mae") %>%
      fit(mpg ~ ., data = mtcars[-(1:4), ]),
    regex = NA
  )

  expect_equal(colnames(reg_fit$fit$evaluation_log)[2], "validation_mae")

  expect_error(
    reg_fit <-
      boost_tree(trees = 20, mode = "regression") %>%
      set_engine("xgboost", eval_metric = "mae") %>%
      fit(mpg ~ ., data = mtcars[-(1:4), ]),
    regex = NA
  )

  expect_equal(colnames(reg_fit$fit$evaluation_log)[2], "training_mae")

  expect_error(
    reg_fit <-
      boost_tree(trees = 20, mode = "regression") %>%
      set_engine("xgboost", validation = 3) %>%
      fit(mpg ~ ., data = mtcars[-(1:4), ]),
    regex = "`validation` should be on"
  )

})


# ------------------------------------------------------------------------------

test_that('early stopping', {
  skip_if_not_installed("xgboost")
  set.seed(233456)
  expect_error(
    reg_fit <-
      boost_tree(trees = 200, stop_iter = 5, mode = "regression") %>%
      set_engine("xgboost", validation = .1) %>%
      fit(mpg ~ ., data = mtcars[-(1:4), ]),
    regex = NA
  )

  expect_equal(reg_fit$fit$niter - reg_fit$fit$best_iteration, 5)
  expect_true(reg_fit$fit$niter < 200)

  expect_error(
    reg_fit <-
      boost_tree(trees = 20, mode = "regression") %>%
      set_engine("xgboost", validation = .1, eval_metric = "mae") %>%
      fit(mpg ~ ., data = mtcars[-(1:4), ]),
    regex = NA
  )

 expect_warning(
    reg_fit <-
      boost_tree(trees = 20, stop_iter = 30, mode = "regression") %>%
      set_engine("xgboost", validation = .1) %>%
      fit(mpg ~ ., data = mtcars[-(1:4), ]),
    regex = "`early_stop` was reduced to 19"
  )
 expect_error(
   reg_fit <-
     boost_tree(trees = 20, stop_iter = 0, mode = "regression") %>%
     set_engine("xgboost", validation = .1) %>%
     fit(mpg ~ ., data = mtcars[-(1:4), ]),
   regex = "`early_stop` should be on"
 )
})


## -----------------------------------------------------------------------------

test_that('xgboost data conversion', {
  skip_if_not_installed("xgboost")

  mtcar_x <- mtcars[, -1]
  mtcar_mat <- as.matrix(mtcar_x)
  mtcar_smat <- Matrix::Matrix(mtcar_mat, sparse = TRUE)

  expect_error(from_df <- parsnip:::as_xgb_data(mtcar_x, mtcars$mpg), regexp = NA)
  expect_true(inherits(from_df$data, "xgb.DMatrix"))
  expect_true(inherits(from_df$watchlist$training, "xgb.DMatrix"))

  expect_error(from_mat <- parsnip:::as_xgb_data(mtcar_mat, mtcars$mpg), regexp = NA)
  expect_true(inherits(from_mat$data, "xgb.DMatrix"))
  expect_true(inherits(from_mat$watchlist$training, "xgb.DMatrix"))

  expect_error(from_sparse <- parsnip:::as_xgb_data(mtcar_smat, mtcars$mpg), regexp = NA)
  expect_true(inherits(from_mat$data, "xgb.DMatrix"))
  expect_true(inherits(from_mat$watchlist$training, "xgb.DMatrix"))

  expect_error(from_df <- parsnip:::as_xgb_data(mtcar_x, mtcars$mpg, validation = .1), regexp = NA)
  expect_true(inherits(from_df$data, "xgb.DMatrix"))
  expect_true(inherits(from_df$watchlist$validation, "xgb.DMatrix"))
  expect_true(nrow(from_df$data) > nrow(from_df$watchlist$validation))

  expect_error(from_mat <- parsnip:::as_xgb_data(mtcar_mat, mtcars$mpg, validation = .1), regexp = NA)
  expect_true(inherits(from_mat$data, "xgb.DMatrix"))
  expect_true(inherits(from_mat$watchlist$validation, "xgb.DMatrix"))
  expect_true(nrow(from_mat$data) > nrow(from_mat$watchlist$validation))

  expect_error(from_sparse <- parsnip:::as_xgb_data(mtcar_smat, mtcars$mpg, validation = .1), regexp = NA)
  expect_true(inherits(from_mat$data, "xgb.DMatrix"))
  expect_true(inherits(from_mat$watchlist$validation, "xgb.DMatrix"))
  expect_true(nrow(from_sparse$data) > nrow(from_sparse$watchlist$validation))

})


test_that('xgboost data and sparse matrices', {
  skip_if_not_installed("xgboost")

  mtcar_x <- mtcars[, -1]
  mtcar_mat <- as.matrix(mtcar_x)
  mtcar_smat <- Matrix::Matrix(mtcar_mat, sparse = TRUE)

  xgb_spec <-
    boost_tree(trees = 10) %>%
    set_engine("xgboost") %>%
    set_mode("regression")

  set.seed(1)
  from_df <- xgb_spec %>% fit_xy(mtcar_x, mtcars$mpg)
  set.seed(1)
  from_mat <- xgb_spec %>% fit_xy(mtcar_mat, mtcars$mpg)
  set.seed(1)
  from_sparse <- xgb_spec %>% fit_xy(mtcar_smat, mtcars$mpg)

  expect_equal(from_df$fit, from_mat$fit)
  expect_equal(from_df$fit, from_sparse$fit)

})


## -----------------------------------------------------------------------------

test_that('argument checks for data dimensions', {

  skip_if_not_installed("C50")

  data(penguins, package = "modeldata")
  penguins <- na.omit(penguins)

  spec <-
    boost_tree(mtry = 1000, min_n = 1000, trees = 5) %>%
    set_engine("xgboost") %>%
    set_mode("classification")

  penguins_dummy <- model.matrix(species ~ ., data = penguins)
  penguins_dummy <- as.data.frame(penguins_dummy[, -1])

  f_fit  <- spec %>% fit(species ~ ., data = penguins)
  xy_fit <- spec %>% fit_xy(x = penguins_dummy, y = penguins$species)

  expect_equal(f_fit$fit$params$colsample_bytree, 1)
  expect_equal(f_fit$fit$params$min_child_weight, nrow(penguins))
  expect_equal(xy_fit$fit$params$colsample_bytree, 1)
  expect_equal(xy_fit$fit$params$min_child_weight, nrow(penguins))

})


