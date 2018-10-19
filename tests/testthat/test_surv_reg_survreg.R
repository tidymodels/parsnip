library(testthat)
library(parsnip)
library(rlang)
library(survival)

# ------------------------------------------------------------------------------

basic_form <- Surv(time, status) ~ group
complete_form <- Surv(time) ~ group

surv_basic <- surv_reg()
surv_lnorm <- surv_reg(dist = "lognormal")

ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

test_that('survival execution', {

  expect_error(
    res <- fit(
      surv_basic,
      Surv(time, status) ~ age + sex,
      data = lung,
      control = ctrl,
      engine = "survreg"
    ),
    regexp = NA
  )
  expect_error(
    res <- fit(
      surv_lnorm,
      Surv(time) ~ age + sex,
      data = lung,
      control = ctrl,
      engine = "survreg"
    ),
    regexp = NA
  )
  expect_error(
    res <- fit_xy(
      surv_basic,
      x = lung[, c("age", "sex")],
      y = lung$time,
      engine = "survreg",
      control = ctrl
    )
  )
})
