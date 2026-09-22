test_that('protected arguments in `opts` are dropped with a warning', {
  lm_fit <- linear_reg() |> set_engine("lm") |> fit(mpg ~ ., data = mtcars)
  baseline <- predict_raw(lm_fit, mtcars)

  expect_snapshot(
    res <- predict_raw(lm_fit, mtcars, opts = list(newdata = mtcars))
  )
  expect_equal(res, baseline)

  expect_snapshot(
    res <- predict_raw(
      lm_fit,
      mtcars,
      opts = list(newdata = mtcars, object = 1)
    )
  )
  expect_equal(res, baseline)

  expect_snapshot(
    res <- predict(lm_fit, mtcars, type = "raw", opts = list(newdata = mtcars))
  )
  expect_equal(res, baseline)
})

test_that('unprotected `opts` reach the prediction call', {
  lm_fit <- linear_reg() |> set_engine("lm") |> fit(mpg ~ ., data = mtcars)

  expect_no_condition(
    res <- predict_raw(lm_fit, mtcars, opts = list(type = "terms"))
  )
  expect_equal(dim(res), c(32L, 10L))
  expect_equal(colnames(res), setdiff(names(mtcars), "mpg"))
})
