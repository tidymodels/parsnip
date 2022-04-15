
test_that("primary argument", {
  normal <- survival_reg(dist = "lnorm")
  expect_equal(
    normal$args,
    list(dist = rlang::quo("lnorm"))
  )

  dist_v <- survival_reg(dist = tune())
  expect_equal(
    dist_v$args,
    list(dist = rlang::quo(tune()))
  )
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
