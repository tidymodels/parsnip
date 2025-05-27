test_that("update methods work (eg: linear_reg)", {
  expr1 <- linear_reg() |> set_engine("lm", model = FALSE)
  expr2 <- linear_reg() |> set_engine("glmnet", nlambda = tune())
  expr3 <- linear_reg(mixture = 0, penalty = tune()) |> set_engine("glmnet", nlambda = tune())
  expr4 <- linear_reg(mixture = 0) |> set_engine("glmnet", nlambda = 10)
  expr5 <- linear_reg() |> set_engine("glm", family = "gaussian")

  param_tibb <- tibble::tibble(mixture = 1/2, penalty = 1)
  param_list <- as.list(param_tibb)

  expect_snapshot(expr1 |> update(mixture = 0))
  expect_snapshot(expr1 |> update(mixture = 0, fresh = TRUE))
  expect_snapshot(expr2 |> update(nlambda = 10))
  expect_snapshot(expr3 |> update(mixture = 1, nlambda = 10))
  expect_snapshot(expr3 |> update(mixture = 1, nlambda = 10, fresh = TRUE))
  expect_snapshot(expr3 |> update(nlambda = 10))
  expect_snapshot(expr3 |> update(nlambda = 10, fresh = TRUE))
  expect_snapshot(expr4 |> update(param_tibb))
  expect_snapshot(expr4 |> update(param_list))
  expect_snapshot(expr4 |> update(param_tibb, fresh = TRUE))
  expect_snapshot(expr4 |> update(param_list, fresh = TRUE))
  expect_snapshot(expr5 |> update(family = "poisson"))
  expect_snapshot(expr5 |> update(family = "poisson", fresh = TRUE))
})

test_that("update methods prompt informatively", {
  # engine arguments passed in param
  expr1 <- linear_reg(mixture = 0) |> set_engine("glmnet", nlambda = 10)

  param_tibb <- tibble::tibble(mixture = 1/2, nlambda = 5)
  param_list <- as.list(param_tibb)

  expect_snapshot(error = TRUE, expr1 |> update(param_tibb))
  expect_snapshot(error = TRUE, expr1 |> update(param_list))
  expect_snapshot(error = TRUE, expr1 |> update(parameters = "wat"))
  expect_snapshot(error = TRUE, expr1 |> update(parameters = tibble::tibble(wat = "wat")))

  # nonexistent main or eng args
  expect_snapshot(error = TRUE, linear_reg() |> update(boop = 0))
})
