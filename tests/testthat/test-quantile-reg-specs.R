test_that('test mode setting for quantile regression', {

  expect_snapshot(
    linear_reg() %>%
      set_engine("quantreg") %>%
      set_mode("regression"),
    error = TRUE)

  expect_snapshot(
    linear_reg() %>%
      set_engine("quantreg") %>%
      set_mode("quantile regression"),
    error = TRUE)

  expect_snapshot(
    linear_reg() %>%
      set_engine("quantreg") %>%
      set_mode("quantile regression", quantile_level = 2),
    error = TRUE)

  # TODO this needs to be improved
  expect_snapshot(
    linear_reg() %>%
      set_engine("quantreg") %>%
      set_mode("quantile regression", quantile_level = 1:2),
    error = TRUE)

  expect_snapshot(
    linear_reg() %>%
      set_engine("quantreg") %>%
      set_mode("quantile regression", quantile_level = NA_real_),
    error = TRUE)
})
