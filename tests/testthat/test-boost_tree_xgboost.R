skip_if_not_installed("modeldata")

hpc <- hpc_data[1:150, c(2:5, 8)]

num_pred <- names(hpc)[1:4]

hpc_xgboost <-
  boost_tree(trees = 2, mode = "classification") |>
  set_engine("xgboost")

# ------------------------------------------------------------------------------

test_that('xgboost execution, classification', {
  skip_if_not_installed("xgboost")
  skip_on_cran()

  ctrl$verbosity <- 0L

  set.seed(1)
  wts <- ifelse(runif(nrow(hpc)) < .1, 0, 1)
  wts <- importance_weights(wts)

  expect_no_condition({
    set.seed(1)
    res_f <- parsnip::fit(
      hpc_xgboost,
      class ~ compounds + input_fields,
      data = hpc,
      control = ctrl
    )
  })
  expect_no_condition({
    set.seed(1)
    res_xy <- parsnip::fit_xy(
      hpc_xgboost,
      x = hpc[, c("compounds", "input_fields")],
      y = hpc$class,
      control = ctrl
    )
  })
  expect_no_condition({
    set.seed(1)
    res_f_wts <- parsnip::fit(
      hpc_xgboost,
      class ~ compounds + input_fields,
      data = hpc,
      control = ctrl,
      case_weights = wts
    )
  })
  expect_no_condition({
    set.seed(1)
    res_xy_wts <- parsnip::fit_xy(
      hpc_xgboost,
      x = hpc[, c("compounds", "input_fields")],
      y = hpc$class,
      control = ctrl,
      case_weights = wts
    )
  })

  expect_equal(res_f$fit$evaluation_log,     res_xy$fit$evaluation_log)
  expect_equal(res_f_wts$fit$evaluation_log, res_xy_wts$fit$evaluation_log)
  # Check to see if the case weights had an effect
  expect_true(!isTRUE(all.equal(res_f$fit$evaluation_log, res_f_wts$fit$evaluation_log)))

  expect_true(has_multi_predict(res_xy))
  expect_equal(multi_predict_args(res_xy), "trees")

  expect_snapshot(
    error = TRUE,
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
  skip_on_cran()

  library(xgboost)

  ctrl$verbosity <- 0L

  xy_fit <- fit_xy(
    hpc_xgboost,
    x = hpc[, num_pred],
    y = hpc$class,
    control = ctrl
  )

  xy_pred <- predict(extract_fit_engine(xy_fit), newdata = xgb.DMatrix(data = as.matrix(hpc[1:8, num_pred])), type = "class")
  xy_pred <- matrix(xy_pred, ncol = 4, byrow = TRUE)
  xy_pred <- factor(levels(hpc$class)[apply(xy_pred, 1, which.max)], levels = levels(hpc$class))
  expect_equal(xy_pred, predict(xy_fit, new_data = hpc[1:8, num_pred], type = "class")$.pred_class)

  form_fit <- fit(
    hpc_xgboost,
    class ~ .,
    data = hpc,
    control = ctrl
  )

  form_pred <- predict(extract_fit_engine(form_fit), newdata = xgb.DMatrix(data = as.matrix(hpc[1:8, num_pred])), type = "class")
  form_pred <- matrix(form_pred, ncol = 4, byrow = TRUE)
  form_pred <- factor(levels(hpc$class)[apply(form_pred, 1, which.max)], levels = levels(hpc$class))
  expect_equal(form_pred, predict(form_fit, new_data = hpc[1:8, num_pred], type = "class")$.pred_class)
})


# ------------------------------------------------------------------------------

num_pred <- names(mtcars)[3:6]

car_basic <-
  boost_tree(mode = "regression") |>
  set_engine("xgboost")

bad_xgboost_reg <-
  boost_tree(mode = "regression") |>
  set_engine("xgboost", min.node.size = -10)

bad_rf_reg <-
  boost_tree(mode = "regression") |>
  set_engine("xgboost", sampsize = -10)

test_that('xgboost execution, regression', {

  skip_if_not_installed("xgboost")
  skip_on_cran()

  ctrl$verbosity <- 0L

  expect_no_condition(
    res <- parsnip::fit(
      car_basic,
      mpg ~ .,
      data = mtcars,
      control = ctrl
    )
  )

  expect_no_condition(
    res <- parsnip::fit_xy(
      car_basic,
      x = mtcars[, num_pred],
      y = mtcars$mpg,
      control = ctrl
    )
  )

})



test_that('xgboost regression prediction', {

  skip_if_not_installed("xgboost")
  skip_on_cran()

  ctrl$verbosity <- 0L

  xy_fit <- fit_xy(
    car_basic,
    x = mtcars[, -1],
    y = mtcars$mpg,
    control = ctrl
  )

  xy_pred <- predict(extract_fit_engine(xy_fit), newdata = xgb.DMatrix(data = as.matrix(mtcars[1:8, -1])))
  expect_equal(xy_pred, predict(xy_fit, new_data = mtcars[1:8, -1])$.pred)

  form_fit <- fit(
    car_basic,
    mpg ~ .,
    data = mtcars,
    control = ctrl
  )

  form_pred <- predict(extract_fit_engine(form_fit), newdata = xgb.DMatrix(data = as.matrix(mtcars[1:8, -1])))
  expect_equal(form_pred, predict(form_fit, new_data = mtcars[1:8, -1])$.pred)

  expect_equal(extract_fit_engine(form_fit)$params$objective, "reg:squarederror")

})



test_that('xgboost alternate objective', {
  skip_if_not_installed("xgboost")
  skip_on_cran()

  ctrl$verbosity <- 0L

  spec <-
    boost_tree() |>
    set_engine("xgboost", objective = "reg:pseudohubererror") |>
    set_mode("regression")

  xgb_fit <- spec |> fit(mpg ~ ., data = mtcars)
  expect_equal(extract_fit_engine(xgb_fit)$params$objective, "reg:pseudohubererror")
  expect_no_error(xgb_preds <- predict(xgb_fit, new_data = mtcars[1,]))
  expect_s3_class(xgb_preds, "data.frame")

  logregobj <- function(preds, dtrain) {
    labels <- xgboost::getinfo(dtrain, "label")
    preds <- 1 / (1 + exp(-preds))
    grad <- preds - labels
    hess <- preds * (1 - preds)
    return(list(grad = grad, hess = hess))
  }

  spec2 <-
    boost_tree() |>
    set_engine("xgboost", objective = logregobj) |>
    set_mode("classification")

  xgb_fit2 <- spec2 |> fit(vs ~ ., data = mtcars |> mutate(vs = as.factor(vs)))
  expect_equal(rlang::eval_tidy(xgb_fit2$spec$eng_args$objective), logregobj)
  expect_no_error(xgb_preds2 <- predict(xgb_fit2, new_data = mtcars[1,-8]))
  expect_s3_class(xgb_preds2, "data.frame")
})

test_that('submodel prediction', {

  skip_if_not_installed("xgboost")
  skip_on_cran()

  ctrl$verbosity <- 0L

  reg_fit <-
    boost_tree(trees = 20, mode = "regression") |>
    set_engine("xgboost") |>
    fit(mpg ~ ., data = mtcars[-(1:4), ])

  x <-  xgboost::xgb.DMatrix(as.matrix(mtcars[1:4, -1]))

  pruned_pred <- predict(extract_fit_engine(reg_fit), x, iterationrange = c(1, 6))

  mp_res <- multi_predict(reg_fit, new_data = mtcars[1:4, -1], trees = 5)
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred"]], pruned_pred)


  vars <- c("female", "tenure", "total_charges", "phone_service", "monthly_charges")
  class_fit <-
    boost_tree(trees = 20, mode = "classification") |>
    set_engine("xgboost") |>
    fit(churn ~ ., data = wa_churn[-(1:4), c("churn", vars)], control = ctrl)

  x <-  xgboost::xgb.DMatrix(as.matrix(wa_churn[1:4, vars]))

  pred_class <- predict(extract_fit_engine(class_fit), x, iterationrange = c(1, 6))

  mp_res <- multi_predict(class_fit, new_data = wa_churn[1:4, vars], trees = 5, type = "prob")
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred_Yes"]], pred_class)

  expect_snapshot(error = TRUE,
    multi_predict(class_fit, newdata = wa_churn[1:4, vars], trees = 5, type = "prob")
  )
})

# ------------------------------------------------------------------------------

test_that('validation sets', {
  skip_if_not_installed("xgboost")
  skip_on_cran()

  ctrl$verbosity <- 0L

  expect_no_condition(
    reg_fit <-
      boost_tree(trees = 20, mode = "regression") |>
      set_engine("xgboost", validation = .1) |>
      fit(mpg ~ ., data = mtcars[-(1:4), ])
  )

  expect_equal(colnames(extract_fit_engine(reg_fit)$evaluation_log)[2], "validation_rmse")

  expect_no_condition(
    reg_fit <-
      boost_tree(trees = 20, mode = "regression") |>
      set_engine("xgboost", validation = .1, eval_metric = "mae") |>
      fit(mpg ~ ., data = mtcars[-(1:4), ])
  )

  expect_equal(colnames(extract_fit_engine(reg_fit)$evaluation_log)[2], "validation_mae")

  expect_no_condition(
    reg_fit <-
      boost_tree(trees = 20, mode = "regression") |>
      set_engine("xgboost", eval_metric = "mae") |>
      fit(mpg ~ ., data = mtcars[-(1:4), ])
  )

  expect_equal(colnames(extract_fit_engine(reg_fit)$evaluation_log)[2], "training_mae")

  expect_snapshot(
    error = TRUE,
    reg_fit <-
      boost_tree(trees = 20, mode = "regression") |>
      set_engine("xgboost", validation = 3) |>
      fit(mpg ~ ., data = mtcars[-(1:4), ])
  )

})


# ------------------------------------------------------------------------------

test_that('early stopping', {
  skip_if_not_installed("xgboost")
  skip_on_cran()

  ctrl$verbosity <- 0L

  set.seed(233456)
  expect_no_condition(
    reg_fit <-
      boost_tree(trees = 200, stop_iter = 5, mode = "regression") |>
      set_engine("xgboost", validation = .1) |>
      fit(mpg ~ ., data = mtcars[-(1:4), ])
  )

  expect_equal(extract_fit_engine(reg_fit)$niter - extract_fit_engine(reg_fit)$best_iteration, 5)
  expect_true(extract_fit_engine(reg_fit)$niter < 200)

  expect_no_condition(
    reg_fit <-
      boost_tree(trees = 20, mode = "regression") |>
      set_engine("xgboost", validation = .1, eval_metric = "mae") |>
      fit(mpg ~ ., data = mtcars[-(1:4), ])
  )

  expect_snapshot(
    reg_fit <-
      boost_tree(trees = 20, stop_iter = 30, mode = "regression") |>
      set_engine("xgboost", validation = .1) |>
      fit(mpg ~ ., data = mtcars[-(1:4), ])
  )
  expect_snapshot(
    error = TRUE,
    reg_fit <-
      boost_tree(trees = 20, stop_iter = 0, mode = "regression") |>
      set_engine("xgboost", validation = .1) |>
      fit(mpg ~ ., data = mtcars[-(1:4), ])
  )
})


## -----------------------------------------------------------------------------

test_that('xgboost data conversion', {
  skip_if_not_installed("xgboost")
  skip_on_cran()

  mtcar_x <- mtcars[, -1]
  mtcar_mat <- as.matrix(mtcar_x)
  mtcar_smat <- Matrix::Matrix(mtcar_mat, sparse = TRUE)
  wts <- 1:32

  expect_no_condition(from_df <- parsnip:::as_xgb_data(mtcar_x, mtcars$mpg))
  expect_true(inherits(from_df$data, "xgb.DMatrix"))
  expect_true(inherits(from_df$watchlist$training, "xgb.DMatrix"))

  expect_no_condition(from_mat <- parsnip:::as_xgb_data(mtcar_mat, mtcars$mpg))
  expect_true(inherits(from_mat$data, "xgb.DMatrix"))
  expect_true(inherits(from_mat$watchlist$training, "xgb.DMatrix"))

  expect_no_condition(from_sparse <- parsnip:::as_xgb_data(mtcar_smat, mtcars$mpg))
  expect_true(inherits(from_mat$data, "xgb.DMatrix"))
  expect_true(inherits(from_mat$watchlist$training, "xgb.DMatrix"))

  expect_no_condition(from_df <- parsnip:::as_xgb_data(mtcar_x, mtcars$mpg, validation = .1))
  expect_true(inherits(from_df$data, "xgb.DMatrix"))
  expect_true(inherits(from_df$watchlist$validation, "xgb.DMatrix"))
  expect_true(nrow(from_df$data) > nrow(from_df$watchlist$validation))

  expect_no_condition(from_mat <- parsnip:::as_xgb_data(mtcar_mat, mtcars$mpg, validation = .1))
  expect_true(inherits(from_mat$data, "xgb.DMatrix"))
  expect_true(inherits(from_mat$watchlist$validation, "xgb.DMatrix"))
  expect_true(nrow(from_mat$data) > nrow(from_mat$watchlist$validation))

  expect_no_condition(from_sparse <- parsnip:::as_xgb_data(mtcar_smat, mtcars$mpg, validation = .1))
  expect_true(inherits(from_mat$data, "xgb.DMatrix"))
  expect_true(inherits(from_mat$watchlist$validation, "xgb.DMatrix"))
  expect_true(nrow(from_sparse$data) > nrow(from_sparse$watchlist$validation))

  # set event_level for factors

  mtcars_y <- factor(mtcars$mpg < 15, levels = c(TRUE, FALSE), labels = c("low", "high"))
  expect_no_condition(from_df <- parsnip:::as_xgb_data(mtcar_x, mtcars_y))
  expect_equal(xgboost::getinfo(from_df$data, name = "label")[1:5],  rep(0, 5))
  expect_no_condition(from_df <- parsnip:::as_xgb_data(mtcar_x, mtcars_y, event_level = "second"))
  expect_equal(xgboost::getinfo(from_df$data, name = "label")[1:5],  rep(1, 5))

  mtcars_y <- factor(mtcars$mpg < 15, levels = c(TRUE, FALSE, "na"), labels = c("low", "high", "missing"))
  expect_snapshot(
    from_df <- parsnip:::as_xgb_data(mtcar_x, mtcars_y, event_level = "second")
  )

  # case weights added
  expect_no_condition(
    wted <- parsnip:::as_xgb_data(mtcar_x, mtcars$mpg, weights = wts)
  )
  expect_equal(wts, xgboost::getinfo(wted$data, "weight"))
  expect_no_condition(
    wted_val <- parsnip:::as_xgb_data(mtcar_x, mtcars$mpg, weights = wts, validation = 1/4)
  )
  expect_true(all(xgboost::getinfo(wted_val$data, "weight") %in% wts))
  expect_null(xgboost::getinfo(wted_val$watchlist$validation, "weight"))

})


test_that('xgboost data and sparse matrices', {
  skip_if_not_installed("xgboost")
  skip_on_cran()

  ctrl$verbosity <- 0L

  mtcar_x <- mtcars[, -1]
  mtcar_mat <- as.matrix(mtcar_x)
  mtcar_smat <- Matrix::Matrix(mtcar_mat, sparse = TRUE)
  wts <- 1:32

  xgb_spec <-
    boost_tree(trees = 10) |>
    set_engine("xgboost") |>
    set_mode("regression")

  set.seed(1)
  from_df <- xgb_spec |> fit_xy(mtcar_x, mtcars$mpg)
  set.seed(1)
  from_mat <- xgb_spec |> fit_xy(mtcar_mat, mtcars$mpg)
  set.seed(1)
  from_sparse <- xgb_spec |> fit_xy(mtcar_smat, mtcars$mpg)

  from_df$fit$handle <- NULL
  from_mat$fit$handle <- NULL
  from_sparse$fit$handle <- NULL


  expect_equal(extract_fit_engine(from_df), extract_fit_engine(from_mat), ignore_function_env = TRUE)
  expect_equal(extract_fit_engine(from_df), extract_fit_engine(from_sparse), ignore_function_env = TRUE)

  # case weights added
  expect_no_condition(
    wted <- parsnip:::as_xgb_data(mtcar_smat, mtcars$mpg, weights = wts)
  )
  expect_equal(wts, xgboost::getinfo(wted$data, "weight"))
  expect_no_condition(
    wted_val <- parsnip:::as_xgb_data(mtcar_smat, mtcars$mpg, weights = wts, validation = 1/4)
  )
  expect_true(all(xgboost::getinfo(wted_val$data, "weight") %in% wts))
  expect_null(xgboost::getinfo(wted_val$watchlist$validation, "weight"))

})


## -----------------------------------------------------------------------------

test_that('argument checks for data dimensions', {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("xgboost")
  skip_on_cran()

  ctrl$verbosity <- 0L

  data(penguins, package = "modeldata")
  penguins <- na.omit(penguins)

  spec <-
    boost_tree(mtry = 1000, min_n = 1000, trees = 5) |>
    set_engine("xgboost") |>
    set_mode("classification")

  penguins_dummy <- model.matrix(species ~ ., data = penguins)
  penguins_dummy <- as.data.frame(penguins_dummy[, -1])

  expect_snapshot(
    f_fit  <- spec |> fit(species ~ ., data = penguins, control = ctrl)
  )
  expect_snapshot(
    xy_fit <- spec |> fit_xy(x = penguins_dummy, y = penguins$species, control = ctrl)
  )
  expect_equal(extract_fit_engine(f_fit)$params$colsample_bynode, 1)
  expect_equal(extract_fit_engine(f_fit)$params$min_child_weight, nrow(penguins))
  expect_equal(extract_fit_engine(xy_fit)$params$colsample_bynode, 1)
  expect_equal(extract_fit_engine(xy_fit)$params$min_child_weight, nrow(penguins))

})

test_that("fit and prediction with `event_level`", {

  skip_if_not_installed("xgboost")
  skip_on_cran()
  skip_if_not_installed("modeldata")

  ctrl$verbosity <- 0L

  data(penguins, package = "modeldata")
  penguins <- na.omit(penguins[, -c(1:2)])

  train_x <- as.matrix(penguins[-(1:4), -5])
  train_y_1 <- -as.numeric(penguins$sex[-(1:4)]) + 2
  train_y_2 <- as.numeric(penguins$sex[-(1:4)]) - 1

  x_pred <-  xgboost::xgb.DMatrix(as.matrix(penguins[1:4, -5]))

  # event_level = "first"
  set.seed(24)
  fit_p_1 <- boost_tree(trees = 10) |>
    set_engine("xgboost", eval_metric = "auc"
               # event_level = "first" is the default
               ) |>
    set_mode("classification") |>
    fit(sex ~ ., data = penguins[-(1:4), ])

  xgbmat_train_1 <- xgb.DMatrix(data = train_x, label = train_y_1)

  set.seed(24)
  fit_xgb_1 <- xgboost::xgb.train(data = xgbmat_train_1,
                                nrounds = 10,
                                watchlist = list("training" = xgbmat_train_1),
                                objective = "binary:logistic",
                                eval_metric = "auc",
                                verbose = 0)

  expect_equal(extract_fit_engine(fit_p_1)$evaluation_log, fit_xgb_1$evaluation_log)

  pred_xgb_1 <- predict(fit_xgb_1, x_pred)
  pred_p_1 <- predict(fit_p_1, new_data = penguins[1:4, ], type = "prob")
  expect_equal(pred_p_1[[".pred_female"]], pred_xgb_1)

  # event_level = "second"
  set.seed(24)
  fit_p_2 <- boost_tree(trees = 10) |>
    set_engine("xgboost", eval_metric = "auc",
               event_level = "second") |>
    set_mode("classification") |>
    fit(sex ~ ., data = penguins[-(1:4), ])

  xgbmat_train_2 <- xgb.DMatrix(data = train_x, label = train_y_2)

  set.seed(24)
  fit_xgb_2 <- xgboost::xgb.train(data = xgbmat_train_2,
                                  nrounds = 10,
                                  watchlist = list("training" = xgbmat_train_2),
                                  objective = "binary:logistic",
                                  eval_metric = "auc",
                                  verbose = 0)

  expect_equal(extract_fit_engine(fit_p_2)$evaluation_log, fit_xgb_2$evaluation_log)

  pred_xgb_2 <- predict(fit_xgb_2, x_pred)
  pred_p_2 <- predict(fit_p_2, new_data = penguins[1:4, ], type = "prob")
  expect_equal(pred_p_2[[".pred_male"]], pred_xgb_2)

})

test_that("count/proportion parameters", {
  skip_if_not_installed("xgboost")
  skip_on_cran()

  ctrl$verbosity <- 0L

  fit1 <-
    boost_tree(mtry = 7, trees = 4) |>
    set_engine("xgboost") |>
    set_mode("regression") |>
    fit(mpg ~ ., data = mtcars)
  expect_equal(extract_fit_engine(fit1)$params$colsample_bytree, 1)
  expect_equal(extract_fit_engine(fit1)$params$colsample_bynode, 7/(ncol(mtcars) - 1))

  fit2 <-
    boost_tree(mtry = 7, trees = 4) |>
    set_engine("xgboost", colsample_bytree = 4) |>
    set_mode("regression") |>
    fit(mpg ~ ., data = mtcars)
  expect_equal(extract_fit_engine(fit2)$params$colsample_bytree, 4/(ncol(mtcars) - 1))
  expect_equal(extract_fit_engine(fit2)$params$colsample_bynode, 7/(ncol(mtcars) - 1))

  fit3 <-
    boost_tree(trees = 4) |>
    set_engine("xgboost") |>
    set_mode("regression") |>
    fit(mpg ~ ., data = mtcars)
  expect_equal(extract_fit_engine(fit3)$params$colsample_bytree, 1)
  expect_equal(extract_fit_engine(fit3)$params$colsample_bynode, 1)

  fit4 <-
    boost_tree(mtry = .9, trees = 4) |>
    set_engine("xgboost", colsample_bytree = .1, counts = FALSE) |>
    set_mode("regression") |>
    fit(mpg ~ ., data = mtcars)
  expect_equal(extract_fit_engine(fit4)$params$colsample_bytree, .1)
  expect_equal(extract_fit_engine(fit4)$params$colsample_bynode, .9)

  expect_snapshot(
    error = TRUE,
    boost_tree(mtry = .9, trees = 4) |>
      set_engine("xgboost") |>
      set_mode("regression") |>
      fit(mpg ~ ., data = mtcars)
  )

})

test_that('interface to param arguments', {
  skip_if_not_installed("xgboost")
  skip_on_os("windows") # some snapshots different on windows (added spaces)
  skip_on_cran()

  ctrl$verbosity <- 0L

  # define base model spec
  spec_base <-
    boost_tree() |>
    set_mode("regression")

  # pass params to params argument (bad)
  spec_1 <-
    spec_base |>
    set_engine("xgboost", params = list(eval_metric = "mae"))

  expect_snapshot_warning(
    fit_1 <- spec_1 |> fit(mpg ~ ., data = mtcars),
    class = "xgboost_params_warning"
  )

  expect_equal(extract_fit_engine(fit_1)$params$eval_metric, "mae")

  # pass params as main argument (good)
  spec_2 <-
    spec_base |>
    set_engine("xgboost", eval_metric = "mae")

  expect_silent(
    fit_2 <- spec_2 |> fit(mpg ~ ., data = mtcars)
  )

  expect_equal(extract_fit_engine(fit_2)$params$eval_metric, "mae")

  # pass objective to params argument (bad)
  spec_3 <-
    spec_base |>
    set_engine("xgboost", params = list(objective = "reg:pseudohubererror"))

  expect_snapshot_warning(
    fit_3 <- spec_3 |> fit(mpg ~ ., data = mtcars),
    class = "xgboost_params_warning"
  )

  expect_equal(extract_fit_engine(fit_3)$params$objective, "reg:pseudohubererror")

  # pass objective as main argument (good)
  spec_4 <-
    spec_base |>
    set_engine("xgboost", objective = "reg:pseudohubererror")

  expect_silent(
    fit_4 <- spec_4 |> fit(mpg ~ ., data = mtcars)
  )

  expect_equal(extract_fit_engine(fit_4)$params$objective, "reg:pseudohubererror")

  # pass a guarded argument as a main argument (bad)
  spec_5 <-
    spec_base |>
    set_engine("xgboost", watchlist = "boop")

  expect_snapshot_warning(
    fit_5 <- spec_5 |> fit(mpg ~ ., data = mtcars),
    class = "xgboost_guarded_warning"
  )

  expect_null(extract_fit_engine(fit_5)$params$watchlist)

  # pass two guarded arguments as main arguments (bad)
  spec_6 <-
    spec_base |>
    set_engine("xgboost", watchlist = "boop", data = "beep")

  expect_snapshot_warning(
    fit_6 <- spec_6 |> fit(mpg ~ ., data = mtcars),
    class = "xgboost_guarded_warning"
  )

  expect_null(extract_fit_engine(fit_5)$params$watchlist)

  # pass a guarded argument as params argument (bad)
  spec_7 <-
    spec_base |>
    set_engine("xgboost", params = list(gamma = 0.1))

  expect_snapshot_warning(
    fit_7 <- spec_7 |> fit(mpg ~ ., data = mtcars),
    class = "xgboost_params_warning"
  )

  expect_equal(extract_fit_engine(fit_5)$params$gamma, 0)
})
