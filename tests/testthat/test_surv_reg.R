test_that('updating', {
  rlang::local_options(lifecycle_verbosity = "quiet")

  expr1 <- surv_reg() %>% set_engine("flexsurv", cl = tune())

  param_tibb <- tibble::tibble(dist = "weibull")
  param_list <- as.list(param_tibb)

  expect_snapshot(expr1 %>% update(dist = "lnorm", cl = 0.99))
  expect_snapshot(expr1 %>% update(param_tibb))
  expect_snapshot(expr1 %>% update(param_list))
})

test_that('bad input', {
  rlang::local_options(lifecycle_verbosity = "quiet")

  expect_error(surv_reg(mode = ", classification"))
  expect_error(translate(surv_reg() %>% set_engine("wat")))
  expect_error(translate(surv_reg() %>% set_engine(NULL)))
})

test_that("deprecation warning", {
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_warning(surv_reg())
})
