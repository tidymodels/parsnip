library(testthat)
library(parsnip)
library(rlang)
library(tibble)
library(dplyr)

# ------------------------------------------------------------------------------

context("keras logistic regression")
source("helpers.R")
source(test_path("helper-objects.R"))
hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

set.seed(352)
dat <- hpc[order(runif(150)),]

tr_dat <- dat[1:140, ]
te_dat <- dat[141:150, ]

# ------------------------------------------------------------------------------

basic_mod <-
  multinom_reg() %>%
  set_engine("keras", epochs = 50, verbose = 0)

reg_mod <-
  multinom_reg(penalty = 0.1) %>%
  set_engine("keras", epochs = 50, verbose = 0)

ctrl <- control_parsnip(verbosity = 0, catch = FALSE)

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
        x = tr_dat[, -5],
        y = tr_dat$class
      ),
    regexp = NA
  )

  set.seed(257)
  expect_error(
    fit2 <-
      fit_xy(
        basic_mod,
        control = ctrl,
        x = tr_dat[, -5],
        y = tr_dat$class
      ),
    regexp = NA
  )
  expect_equal(
    unlist(keras::get_weights(fit1$fit)),
    unlist(keras::get_weights(fit2$fit)),
    tolerance = .1
  )

  expect_error(
    fit(
      basic_mod,
      class ~ .,
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
        x = tr_dat[, -5],
        y = tr_dat$class
      ),
    regexp = NA
  )

  expect_error(
    fit(
      reg_mod,
      class ~ .,
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
      x = tr_dat[, -5],
      y = tr_dat$class
    )

  keras_raw <-
    predict(lr_fit$fit, as.matrix(te_dat[, -5]))
  keras_pred <-
    tibble(.pred_class = apply(keras_raw, 1, which.max)) %>%
    mutate(.pred_class = factor(lr_fit$lvl[.pred_class], levels = lr_fit$lvl))

  parsnip_pred <- predict(lr_fit, te_dat[, -5])
  expect_equal(as.data.frame(keras_pred), as.data.frame(parsnip_pred))

  set.seed(257)
  plrfit <-
    fit_xy(
      reg_mod,
      control = ctrl,
      x = tr_dat[, -5],
      y = tr_dat$class
    )

  keras_raw <-
    predict(plrfit$fit, as.matrix(te_dat[, -5]))
  keras_pred <-
    tibble(.pred_class = apply(keras_raw, 1, which.max)) %>%
    mutate(.pred_class = factor(plrfit$lvl[.pred_class], levels = plrfit$lvl))
  parsnip_pred <- predict(plrfit, te_dat[, -5])
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
      x = tr_dat[, -5],
      y = tr_dat$class
    )

  keras_pred <-
    keras::predict_proba(lr_fit$fit, as.matrix(te_dat[, -5])) %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(paste0(".pred_", lr_fit$lvl))

  parsnip_pred <- predict(lr_fit, te_dat[, -5], type = "prob")
  expect_equal(as.data.frame(keras_pred), as.data.frame(parsnip_pred))

  set.seed(257)
  plrfit <-
    fit_xy(
      reg_mod,
      control = ctrl,
      x = tr_dat[, -5],
      y = tr_dat$class
    )

  keras_pred <-
    keras::predict_proba(plrfit$fit, as.matrix(te_dat[, -5])) %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(paste0(".pred_", lr_fit$lvl))
  parsnip_pred <- predict(plrfit, te_dat[, -5], type = "prob")
  expect_equal(as.data.frame(keras_pred), as.data.frame(parsnip_pred))

})


