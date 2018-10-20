library(testthat)
library(parsnip)
library(rlang)
library(survival)

# ------------------------------------------------------------------------------

basic_form <- Surv(recyrs, censrec) ~ group
complete_form <- Surv(recyrs) ~ group

surv_basic <- surv_reg()
ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

# ------------------------------------------------------------------------------

test_that('flexsurv execution', {
  skip_if_not_installed("flexsurv")

  library(flexsurv)
  data(bc)

  set.seed(4566)
  bc$group2 <- bc$group

  expect_error(
    res <- fit(
      surv_basic,
      Surv(recyrs, censrec) ~ group,
      data = bc,
      control = ctrl,
      engine = "flexsurv"
    ),
    regexp = NA
  )
  expect_error(
    res <- fit(
      surv_basic,
      Surv(recyrs) ~ group,
      data = bc,
      control = ctrl,
      engine = "flexsurv"
    ),
    regexp = NA
  )
  expect_error(
    res <- fit_xy(
      surv_basic,
      x = bc[, "group", drop = FALSE],
      y = bc$recyrs,
      engine = "flexsurv",
      control = ctrl
    )
  )
})

test_that('flexsurv prediction', {
  skip_if_not_installed("flexsurv")

  library(flexsurv)
  data(bc)

  set.seed(4566)
  bc$group2 <- bc$group

  res <- fit(
    surv_basic,
    Surv(recyrs, censrec) ~ group,
    data = bc,
    control = ctrl,
    engine = "flexsurv"
  )
  exp_pred <- summary(res$fit, head(bc), type = "mean")
  exp_pred <- do.call("rbind", unclass(exp_pred))
  exp_pred <- tibble(.pred = exp_pred$est)
  expect_equal(exp_pred, predict(res, head(bc)))
})
