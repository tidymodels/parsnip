
test_that("primary argument", {
  new_empty_quosure <- function(expr) {
    rlang::new_quosure(expr, env = rlang::empty_env())
  }

  normal <- survival_reg(dist = "lnorm")
  expect_equal(
    normal$args,
    list(dist = new_empty_quosure("lnorm"))
  )

  dist_v <- survival_reg(dist = varying())
  expect_equal(
    dist_v$args,
    list(dist = new_empty_quosure(varying()))
  )
})

test_that("updating", {
  new_empty_quosure <- function(expr) {
    rlang::new_quosure(expr, env = rlang::empty_env())
  }

  basic <- survival_reg()

  update_chr <- update(basic, dist = "lnorm")
  expect_equal(
    update_chr$args,
    list(dist = new_empty_quosure("lnorm"))
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
