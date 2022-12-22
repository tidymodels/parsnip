test_that("updating", {
  expect_snapshot(
    poisson_reg(penalty = 1) %>%
      set_engine("glmnet", lambda.min.ratio = 0.001) %>%
      update(mixture = tune())
  )
})

test_that("bad input", {
  expect_snapshot(error = TRUE, poisson_reg(mode = "bogus"))
  expect_snapshot(error = TRUE, translate(poisson_reg(mode = "regression"), engine = NULL))
  expect_snapshot(error = TRUE, poisson_reg(formula = y ~ x))
})
