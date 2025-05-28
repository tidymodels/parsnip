

# ------------------------------------------------------------------------------

test_that('survival execution', {
  skip_if_not_installed("survival")
  rlang::local_options(lifecycle_verbosity = "quiet")
  surv_basic <- surv_reg() |> set_engine("survival")
  surv_lnorm <- surv_reg(dist = "lognormal") |> set_engine("survival")

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
  skip_if_not_installed("survival")

  rlang::local_options(lifecycle_verbosity = "quiet")
  surv_basic <- surv_reg() |> set_engine("survival")
  surv_lnorm <- surv_reg(dist = "lognormal") |> set_engine("survival")

  res <- fit(
    surv_basic,
    survival::Surv(time, status) ~ age + sex,
    data = lung,
    control = ctrl
  )
  exp_pred <- predict(extract_fit_engine(res), head(lung))
  exp_pred <- tibble(.pred = unname(exp_pred))
  expect_equal(exp_pred, predict(res, head(lung)))
})


