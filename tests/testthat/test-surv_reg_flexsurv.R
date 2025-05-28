

# ------------------------------------------------------------------------------

test_that('flexsurv execution', {
  skip_if_not_installed("flexsurv")
  skip_if_not_installed("survival")

  rlang::local_options(lifecycle_verbosity = "quiet")
  surv_basic <- surv_reg() |> set_engine("flexsurv")

  expect_no_condition(
    res <- fit(
      surv_basic,
      survival::Surv(time, status) ~ age,
      data = lung,
      control = ctrl
    )
  )
  expect_no_condition(
    res <- fit(
      surv_basic,
      survival::Surv(time) ~ age,
      data = lung,
      control = ctrl
    )
  )
  expect_false(has_multi_predict(res))
  expect_equal(multi_predict_args(res), NA_character_)

  expect_snapshot(
    error = TRUE,
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
  skip_if_not_installed("survival")


  rlang::local_options(lifecycle_verbosity = "quiet")
  surv_basic <- surv_reg() |> set_engine("flexsurv")

  res <- fit(
    surv_basic,
    survival::Surv(time, status) ~ age,
    data = lung,
    control = ctrl
  )
  exp_pred <- summary(extract_fit_engine(res), head(lung), type = "mean")
  exp_pred <- do.call("rbind", unclass(exp_pred))
  exp_pred <- tibble(.pred = exp_pred$est)
  expect_equal(exp_pred, predict(res, head(lung)))
})
