library(testthat)
library(parsnip)
library(rlang)
library(tibble)
library(dplyr)

# ------------------------------------------------------------------------------

context("nnet multinomial regression")
source("helpers.R")

# ------------------------------------------------------------------------------

set.seed(352)
dat <- iris[order(runif(nrow(iris))),]

tr_dat <- dat[1:140, ]
te_dat <- dat[141:150, ]

# ------------------------------------------------------------------------------

basic_mod <-
  multinom_reg() %>%
  set_engine("nnet", penalty = .1)

ctrl <- control_parsnip(verbosity = 0, catch = FALSE)

# ------------------------------------------------------------------------------

test_that('model fitting', {
  skip_if_not_installed("nnet")

  set.seed(257)
  expect_error(
    fit1 <-
      fit_xy(
        basic_mod,
        control = ctrl,
        x = tr_dat[, -5],
        y = tr_dat$Species
      ),
    regexp = NA
  )
  expect_output(print(fit1), "parsnip model object")

  set.seed(257)
  expect_error(
    fit2 <-
      fit_xy(
        basic_mod,
        control = ctrl,
        x = tr_dat[, -5],
        y = tr_dat$Species
      ),
    regexp = NA
  )
  fit1$elapsed <- fit2$elapsed
  expect_equal(fit1, fit2)

  expect_error(
    fit(
      basic_mod,
      Species ~ .,
      data = tr_dat,
      control = ctrl
    ),
    regexp = NA
  )

})


test_that('classification prediction', {
  skip_if_not_installed("nnet")

  set.seed(257)
  lr_fit <-
    fit_xy(
      basic_mod,
      control = ctrl,
      x = tr_dat[, -5],
      y = tr_dat$Species
    )

  nnet_pred <-
    predict(lr_fit$fit, as.matrix(te_dat[, -5]))

  parsnip_pred <- predict(lr_fit, te_dat[, -5])
  expect_equal(nnet_pred, parsnip_pred$.pred_class)

})


test_that('classification probabilities', {
  skip_if_not_installed("nnet")

  set.seed(257)
  lr_fit <-
    fit_xy(
      basic_mod,
      control = ctrl,
      x = tr_dat[, -5],
      y = tr_dat$Species
    )

  nnet_pred <-
    predict(lr_fit$fit, as.matrix(te_dat[, -5]), type = "prob") %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(paste0(".pred_", lr_fit$lvl))

  parsnip_pred <- predict(lr_fit, te_dat[, -5], type = "prob")
  expect_equal(as.data.frame(nnet_pred), as.data.frame(parsnip_pred))

})


