library(testthat)
library(parsnip)
library(rlang)
library(tibble)
library(dplyr)

# ------------------------------------------------------------------------------

context("keras logistic regression")
source("helpers.R")

# ------------------------------------------------------------------------------

data("lending_club")
set.seed(352)
dat <-
  lending_club %>%
  group_by(Class) %>%
  sample_n(500) %>%
  ungroup() %>%
  dplyr::select(Class, funded_amnt, int_rate)
dat <- dat[order(runif(nrow(dat))),]

tr_dat <- dat[1:995, ]
te_dat <- dat[996:1000, ]

# ------------------------------------------------------------------------------

basic_mod <-
  logistic_reg() %>%
  set_engine("keras", epochs = 50, verbose = 0)

reg_mod <-
  logistic_reg(penalty = 0.1) %>%
  set_engine("keras", epochs = 50, verbose = 0)

ctrl <- fit_control(verbosity = 0, catch = FALSE)

# ------------------------------------------------------------------------------

test_that('model fitting', {
  skip_on_cran()
  skip_if_not_installed("keras")

  set.seed(257)
  expect_error(
    fit1 <-
      fit_xy(
      basic_mod,
      control = ctrl,
      x = tr_dat[, -1],
      y = tr_dat$Class
    ),
    regexp = NA
  )

  set.seed(257)
  expect_error(
    fit2 <-
      fit_xy(
        basic_mod,
        control = ctrl,
        x = tr_dat[, -1],
        y = tr_dat$Class
      ),
    regexp = NA
  )
  expect_equal(fit1, fit2)

  expect_error(
    fit(
      basic_mod,
      Class ~ .,
      data = tr_dat,
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    fit1 <-
      fit_xy(
        reg_mod,
        control = ctrl,
        x = tr_dat[, -1],
        y = tr_dat$Class
      ),
    regexp = NA
  )

  expect_error(
    fit(
      reg_mod,
      Class ~ .,
      data = tr_dat,
      control = ctrl
    ),
    regexp = NA
  )

})


test_that('classification prediction', {
  skip_on_cran()
  skip_if_not_installed("keras")

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
    tibble(.pred_class = apply(keras_raw, 1, which.max)) %>%
    mutate(.pred_class = factor(lr_fit$lvl[.pred_class], levels = lr_fit$lvl))

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
    tibble(.pred_class = apply(keras_raw, 1, which.max)) %>%
    mutate(.pred_class = factor(plrfit$lvl[.pred_class], levels = plrfit$lvl))
  parsnip_pred <- predict(plrfit, te_dat[, -1])
  expect_equal(as.data.frame(keras_pred), as.data.frame(parsnip_pred))

})


test_that('classification probabilities', {
  skip_on_cran()
  skip_if_not_installed("keras")

  library(keras)

  set.seed(257)
  lr_fit <-
    fit_xy(
      basic_mod,
      control = ctrl,
      x = tr_dat[, -1],
      y = tr_dat$Class
    )

  keras_pred <-
    predict_proba(lr_fit$fit, as.matrix(te_dat[, -1])) %>%
    as_tibble() %>%
    setNames(paste0(".pred_", lr_fit$lvl))

  parsnip_pred <- predict(lr_fit, te_dat[, -1], type = "prob")
  expect_equal(as.data.frame(keras_pred), as.data.frame(parsnip_pred))

  set.seed(257)
  plrfit <-
    fit_xy(
      reg_mod,
      control = ctrl,
      x = tr_dat[, -1],
      y = tr_dat$Class
    )

  keras_pred <-
    predict_proba(plrfit$fit, as.matrix(te_dat[, -1])) %>%
    as_tibble() %>%
    setNames(paste0(".pred_", lr_fit$lvl))
  parsnip_pred <- predict(plrfit, te_dat[, -1], type = "prob")
  expect_equal(as.data.frame(keras_pred), as.data.frame(parsnip_pred))

})


