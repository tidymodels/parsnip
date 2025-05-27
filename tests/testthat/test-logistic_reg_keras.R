skip_if_not_installed("modeldata")

# ------------------------------------------------------------------------------

basic_mod <-
  logistic_reg() |>
  set_engine("keras", epochs = 50, verbose = 0)

reg_mod <-
  logistic_reg(penalty = 0.1) |>
  set_engine("keras", epochs = 50, verbose = 0)

ctrl <- control_parsnip(verbosity = 0, catch = FALSE)

# ------------------------------------------------------------------------------

test_that('model fitting', {
  skip_on_cran()
  skip_if_not_installed("keras")
  skip_if(!is_tf_ok())

  set.seed(352)
  dat <-
    modeldata::lending_club |>
    dplyr::group_by(Class) |>
    dplyr::sample_n(500) |>
    dplyr::ungroup() |>
    dplyr::select(Class, funded_amnt, int_rate)
  dat <- dat[order(runif(nrow(dat))),]

  tr_dat <- dat[1:995, ]
  te_dat <- dat[996:1000, ]

  set_tf_seed(257)

  expect_no_condition(
    fit1 <-
      fit_xy(
      basic_mod,
      control = ctrl,
      x = tr_dat[, -1],
      y = tr_dat$Class
    )
  )

  set_tf_seed(257)

  expect_no_condition(
    fit2 <-
      fit_xy(
        basic_mod,
        control = ctrl,
        x = tr_dat[, -1],
        y = tr_dat$Class
      )
  )
  expect_equal(
    unlist(keras::get_weights(fit1$fit)),
    unlist(keras::get_weights(fit2$fit)),
    tolerance = .1
  )

  expect_no_condition(
    fit(
      basic_mod,
      Class ~ .,
      data = tr_dat,
      control = ctrl
    )
  )

  expect_no_condition(
    fit1 <-
      fit_xy(
        reg_mod,
        control = ctrl,
        x = tr_dat[, -1],
        y = tr_dat$Class
      )
  )

  expect_no_condition(
    fit(
      reg_mod,
      Class ~ .,
      data = tr_dat,
      control = ctrl
    )
  )

})


test_that('classification prediction', {
  skip_on_cran()
  skip_if_not_installed("keras")
  skip_if(!is_tf_ok())

  set.seed(352)
  dat <-
    modeldata::lending_club |>
    dplyr::group_by(Class) |>
    dplyr::sample_n(500) |>
    dplyr::ungroup() |>
    dplyr::select(Class, funded_amnt, int_rate)
  dat <- dat[order(runif(nrow(dat))),]

  tr_dat <- dat[1:995, ]
  te_dat <- dat[996:1000, ]

  library(keras)

  set.seed(257)
  lr_fit <-
    fit_xy(
      basic_mod,
      control = ctrl,
      x = tr_dat[, -1],
      y = tr_dat$Class
    )

  keras_raw <-
    predict(lr_fit$fit, as.matrix(te_dat[, -1]))
  keras_pred <-
    tibble::tibble(.pred_class = apply(keras_raw, 1, which.max)) |>
    dplyr::mutate(.pred_class = factor(lr_fit$lvl[.pred_class], levels = lr_fit$lvl))

  parsnip_pred <- predict(lr_fit, te_dat[, -1])
  expect_equal(as.data.frame(keras_pred), as.data.frame(parsnip_pred))

  set.seed(257)
  plrfit <-
    fit_xy(
      reg_mod,
      control = ctrl,
      x = tr_dat[, -1],
      y = tr_dat$Class
    )

  keras_raw <-
    predict(plrfit$fit, as.matrix(te_dat[, -1]))
  keras_pred <-
    tibble(.pred_class = apply(keras_raw, 1, which.max)) |>
    mutate(.pred_class = factor(plrfit$lvl[.pred_class], levels = plrfit$lvl))
  parsnip_pred <- predict(plrfit, te_dat[, -1])
  expect_equal(as.data.frame(keras_pred), as.data.frame(parsnip_pred))

})


test_that('classification probabilities', {
  skip_on_cran()
  skip_if_not_installed("keras")
  skip_if(!is_tf_ok())

  set.seed(352)
  dat <-
    modeldata::lending_club |>
    dplyr::group_by(Class) |>
    dplyr::sample_n(500) |>
    dplyr::ungroup() |>
    dplyr::select(Class, funded_amnt, int_rate)
  dat <- dat[order(runif(nrow(dat))),]

  tr_dat <- dat[1:995, ]
  te_dat <- dat[996:1000, ]

  library(keras)

  set_tf_seed(257)

  lr_fit <-
    fit_xy(
      basic_mod,
      control = ctrl,
      x = tr_dat[, -1],
      y = tr_dat$Class
    )

  keras_pred <- predict(lr_fit$fit, as.matrix(te_dat[, -1]))
  colnames(keras_pred) <- paste0(".pred_", lr_fit$lvl)
  keras_pred <- as_tibble(keras_pred)

  parsnip_pred <- predict(lr_fit, te_dat[, -1], type = "prob")
  expect_equal(as.data.frame(keras_pred), as.data.frame(parsnip_pred))

  set_tf_seed(257)

  plrfit <-
    fit_xy(
      reg_mod,
      control = ctrl,
      x = tr_dat[, -1],
      y = tr_dat$Class
    )

  keras_pred <- predict(plrfit$fit, as.matrix(te_dat[, -1]))
  colnames(keras_pred) <- paste0(".pred_", lr_fit$lvl)
  keras_pred <- tibble::as_tibble(keras_pred)

  parsnip_pred <- predict(plrfit, te_dat[, -1], type = "prob")
  expect_equal(as.data.frame(keras_pred), as.data.frame(parsnip_pred))

})
