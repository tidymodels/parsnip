test_that("prepare_data() re-levels factors using the training ptype", {
  dat <- data.frame(
    y = c(1.2, 3.4, 2.2, 5.1, 4.0, 2.9),
    a = factor(c("x", "y", "z", "x", "y", "z")),
    b = c(1, 5, 2, 8, 3, 4)
  )
  lm_fit <- fit_xy(linear_reg(), x = dat[c("a", "b")], y = dat$y)

  reordered <- data.frame(a = factor(c("z", "x"), levels = c("z", "x")), b = 1)
  res <- prepare_data(lm_fit, reordered)
  expect_identical(levels(res$a), c("x", "y", "z"))
  expect_identical(as.character(res$a), c("z", "x"))

  as_character <- data.frame(a = c("z", "x"), b = 1)
  res <- prepare_data(lm_fit, as_character)
  expect_s3_class(res$a, "factor")
  expect_identical(levels(res$a), c("x", "y", "z"))

  expect_identical(
    predict(lm_fit, reordered),
    predict(lm_fit, as_character)
  )
})

test_that("prepare_data() turns novel levels into missing values", {
  dat <- data.frame(
    y = c(1.2, 3.4, 2.2, 5.1, 4.0, 2.9),
    a = factor(c("x", "y", "z", "x", "y", "z")),
    b = c(1, 5, 2, 8, 3, 4)
  )
  lm_fit <- fit_xy(linear_reg(), x = dat[c("a", "b")], y = dat$y)

  expect_snapshot(
    res <- prepare_data(lm_fit, data.frame(a = c("new", "x"), b = 1))
  )
  expect_identical(as.character(res$a), c(NA, "x"))
})

test_that("prepare_data() works for fits without a ptype", {
  dat <- data.frame(
    y = c(1.2, 3.4, 2.2, 5.1, 4.0, 2.9),
    a = factor(c("x", "y", "z", "x", "y", "z")),
    b = c(1, 5, 2, 8, 3, 4)
  )
  lm_fit <- fit_xy(linear_reg(), x = dat[c("a", "b")], y = dat$y)
  expected <- predict(lm_fit, dat)
  lm_fit$preproc$ptype <- NULL

  expect_identical(predict(lm_fit, dat), expected)
})
