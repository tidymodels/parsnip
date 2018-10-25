library(testthat)
library(parsnip)
library(survival)
library(tibble)

# ------------------------------------------------------------------------------

basic_form <- Surv(time, status) ~ group
complete_form <- Surv(time) ~ group

surv_basic <- surv_reg() %>% set_engine("survreg")
surv_lnorm <- surv_reg(dist = "lognormal") %>% set_engine("survreg")

ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

# ------------------------------------------------------------------------------

test_that('survival execution', {

  expect_error(
    res <- fit(
      surv_basic,
      Surv(time, status) ~ age + sex,
      data = lung,
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- fit(
      surv_lnorm,
      Surv(time) ~ age + sex,
      data = lung,
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- fit_xy(
      surv_basic,
      x = lung[, c("age", "sex")],
      y = lung$time,
      control = ctrl
    )
  )
})

test_that('survival prediction', {

  res <- fit(
    surv_basic,
    Surv(time, status) ~ age + sex,
    data = lung,
    control = ctrl
  )
  exp_pred <- predict(res$fit, head(lung))
  exp_pred <- tibble(.pred = unname(exp_pred))
  expect_equal(exp_pred, predict(res, head(lung)))

  exp_quant <- predict(res$fit, head(lung), p = (2:4)/5, type = "quantile")
  exp_quant <-
    apply(exp_quant, 1, function(x)
      tibble(.pred = x, .quantile = (2:4) / 5))
  exp_quant <- tibble(.pred = exp_quant)
  obs_quant <- predict(res, head(lung), type = "quantile", quantile = (2:4)/5)

  expect_equal(as.data.frame(exp_quant), as.data.frame(obs_quant))

})


