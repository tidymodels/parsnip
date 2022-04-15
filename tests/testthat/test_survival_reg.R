
test_that("primary argument", {
  normal <- survival_reg(dist = "lnorm")
  expect_snapshot(normal$args)

  dist_v <- survival_reg(dist = tune())
  expect_snapshot(dist_v$args)
})

test_that("updating", {
  basic <- survival_reg()
  param_tibb <- tibble::tibble(dist = "weibull")
  param_list <- as.list(param_tibb)

  expect_snapshot(basic %>% update(dist = "lnorm"))
  expect_snapshot(basic %>% update(param_tibb))
  expect_snapshot(basic %>% update(param_list))
})

test_that("bad input", {
  expect_error(survival_reg(mode = ", classification"))
})

test_that("wrong fit interface", {
  expect_error(
    survival_reg() %>% fit_xy(),
    "must use the formula interface"
  )
})
