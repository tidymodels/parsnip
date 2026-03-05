test_that("tunable parameters for logistic_reg + glm", {
  expect_snapshot({
    display_tunable_call_info(
      logistic_reg() |> set_engine("glm")
    )
  })
})

test_that("tunable parameters for logistic_reg + glmnet", {
  expect_snapshot({
    display_tunable_call_info(
      logistic_reg() |> set_engine("glmnet")
    )
  })
})

test_that("tunable parameters for logistic_reg + LiblineaR", {
  expect_snapshot({
    display_tunable_call_info(
      logistic_reg() |> set_engine("LiblineaR")
    )
  })
})

test_that("tunable parameters for logistic_reg + spark", {
  expect_snapshot({
    display_tunable_call_info(
      logistic_reg() |> set_engine("spark")
    )
  })
})

test_that("tunable parameters for logistic_reg + keras", {
  expect_snapshot({
    display_tunable_call_info(
      logistic_reg() |> set_engine("keras")
    )
  })
})

test_that("tunable parameters for logistic_reg + stan", {
  expect_snapshot({
    display_tunable_call_info(
      logistic_reg() |> set_engine("stan")
    )
  })
})

test_that("tunable parameters for logistic_reg + brulee", {
  expect_snapshot({
    display_tunable_call_info(
      logistic_reg() |> set_engine("brulee")
    )
  })
})
