test_that('updating', {
  rlang::local_options(lifecycle_verbosity = "quiet")

  expect_snapshot(
    surv_reg() %>%
      set_engine("flexsurv", cl = 0.99) %>%
      update(cl = tune())
  )
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
