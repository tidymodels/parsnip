if (R.Version()$major < "4") {
  data(lung, package = 'survival')
} else {
  data(cancer, package = 'survival')
}

basic_form <- survival::Surv(time, status) ~ group
complete_form <- survival::Surv(time) ~ group

# ------------------------------------------------------------------------------

test_that('survival execution', {
  skip_on_travis()

  rlang::local_options(lifecycle_verbosity = "quiet")
  surv_basic <- surv_reg() %>% set_engine("survival")
  surv_lnorm <- surv_reg(dist = "lognormal") %>% set_engine("survival")

  expect_no_condition(
    res <- fit(
      surv_basic,
      survival::Surv(time, status) ~ age + sex,
      data = lung,
      control = ctrl
    )
  )

  expect_no_condition(
    res <- fit(
      surv_lnorm,
      survival::Surv(time) ~ age + sex,
      data = lung,
      control = ctrl
    )
  )
  expect_snapshot(
    error = TRUE,
    res <- fit_xy(
      surv_basic,
      x = lung[, c("age", "sex")],
      y = lung$time,
      control = ctrl
    )
  )
})

test_that('survival prediction', {
  skip_on_travis()

  rlang::local_options(lifecycle_verbosity = "quiet")
  surv_basic <- surv_reg() %>% set_engine("survival")
  surv_lnorm <- surv_reg(dist = "lognormal") %>% set_engine("survival")

  res <- fit(
    surv_basic,
    survival::Surv(time, status) ~ age + sex,
    data = lung,
    control = ctrl
  )
  exp_pred <- predict(extract_fit_engine(res), head(lung))
  exp_pred <- tibble(.pred = unname(exp_pred))
  expect_equal(exp_pred, predict(res, head(lung)))

  exp_quant <- predict(extract_fit_engine(res), head(lung), p = (2:4)/5, type = "quantile")
  exp_quant <-
    apply(exp_quant, 1, function(x)
      tibble(.pred = x, .quantile = (2:4) / 5))
  exp_quant <- tibble(.pred = exp_quant)
  obs_quant <- predict(res, head(lung), type = "quantile", quantile = (2:4)/5)

  expect_equal(as.data.frame(exp_quant), as.data.frame(obs_quant))

})


