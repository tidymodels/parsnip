test_that("check_args() works", {
  # Here for completeness, no checking is done
  expect_true(TRUE)
})

test_that("dbarts engine allows case weights (#761)", {
  for (mode in c("regression", "classification")) {
    spec <- bart(mode = mode, engine = "dbarts")
    expect_in("weights", names(translate(spec)$method$fit$args))
  }
})

test_that("case weights are passed to dbarts (#761)", {
  skip_if_not_installed("dbarts")

  dat <- mtcars[, c("mpg", "vs", "disp", "hp")]
  dat$vs <- factor(dat$vs)
  wts <- rep(c(1, 100), each = 16)
  spec <- bart(trees = 5, engine = "dbarts")

  reg_fit <- spec |>
    set_mode("regression") |>
    fit(mpg ~ disp + hp, data = dat, case_weights = importance_weights(wts))
  expect_equal(reg_fit$fit$fit$data@weights, wts)

  cls_fit <- spec |>
    set_mode("classification") |>
    fit(vs ~ disp + hp, data = dat, case_weights = importance_weights(wts))
  expect_equal(cls_fit$fit$fit$data@weights, wts)
})
