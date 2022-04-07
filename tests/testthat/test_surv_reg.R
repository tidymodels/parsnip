test_that('updating', {
  rlang::local_options(lifecycle_verbosity = "quiet")

  expr1     <- surv_reg() %>% set_engine("flexsurv", cl = tune())
  expr1_exp <- surv_reg(dist = "lnorm") %>% set_engine("flexsurv", cl = .99)
  expect_equal(update(expr1, dist = "lnorm", cl = 0.99), expr1_exp)

  param_tibb <- tibble::tibble(dist = "weibull")
  param_list <- as.list(param_tibb)

  expr1_updated <- update(expr1, param_tibb)
  expect_equal(expr1_updated$args$dist, "weibull")

  expr1_updated_lst <- update(expr1, param_list)
  expect_equal(expr1_updated_lst$args$dist, "weibull")
})

test_that('bad input', {
  rlang::local_options(lifecycle_verbosity = "quiet")

  expect_error(surv_reg(mode = ", classification"))
  expect_error(translate(surv_reg() %>% set_engine("wat")))
  expect_error(translate(surv_reg() %>% set_engine(NULL)))
})

test_that("deprecation warning", {
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_snapshot(surv_reg())
})
