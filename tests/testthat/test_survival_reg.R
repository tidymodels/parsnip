
test_that("primary argument", {
  normal <- survival_reg(dist = "lnorm")
  expect_equal(
    normal$args,
    list(dist = quo("lnorm"))
  )

  dist_v <- survival_reg(dist = tune())
  expect_equal(
    dist_v$args,
    list(dist = quo(tune()))
  )
})

test_that("updating", {
  basic <- survival_reg()

  update_chr <- update(basic, dist = "lnorm")
  expect_equal(
    update_chr$args,
    list(dist = quo("lnorm"))
  )

  param_tibb <- tibble::tibble(dist = "weibull")
  update_tibb <- update(basic, param_tibb)
  expect_equal(
    update_tibb$args,
    list(dist = "weibull")
  )

  param_list <- as.list(param_tibb)
  update_list <- update(basic, param_list)
  expect_equal(
    update_list$args,
    list(dist = "weibull")
  )

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
