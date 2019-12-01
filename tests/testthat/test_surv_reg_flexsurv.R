library(testthat)
library(parsnip)
library(rlang)
library(survival)
library(tibble)

# ------------------------------------------------------------------------------

source("helper-objects.R")

basic_form <- Surv(time, status) ~ age
complete_form <- Surv(time) ~ age

surv_basic <- surv_reg() %>% set_engine("flexsurv")

# ------------------------------------------------------------------------------

test_that('flexsurv execution', {
  skip_if_not_installed("flexsurv")

  expect_error(
    res <- fit(
      surv_basic,
      Surv(time, status) ~ age,
      data = lung,
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- fit(
      surv_basic,
      Surv(time) ~ age,
      data = lung,
      control = ctrl
    ),
    regexp = NA
  )
  expect_false(has_multi_predict(res))
  expect_equal(multi_predict_args(res), NA_character_)

  expect_error(
    res <- fit_xy(
      surv_basic,
      x = lung[, "age", drop = FALSE],
      y = lung$time,
      control = ctrl
    )
  )
})

test_that('flexsurv prediction', {
  skip_if_not_installed("flexsurv")

  res <- fit(
    surv_basic,
    Surv(time, status) ~ age,
    data = lung,
    control = ctrl
  )
  exp_pred <- summary(res$fit, head(lung), type = "mean")
  exp_pred <- do.call("rbind", unclass(exp_pred))
  exp_pred <- tibble(.pred = exp_pred$est)
  expect_equal(exp_pred, predict(res, head(lung)))
})
