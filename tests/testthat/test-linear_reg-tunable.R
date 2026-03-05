test_that("tunable parameters for linear_reg + lm", {
  expect_snapshot({
    display_tunable_call_info(
      linear_reg() |> set_engine("lm")
    )
  })
})

test_that("tunable parameters for linear_reg + glm", {
  expect_snapshot({
    display_tunable_call_info(
      linear_reg() |> set_engine("glm")
    )
  })
})

test_that("tunable parameters for linear_reg + glmnet", {
  expect_snapshot({
    display_tunable_call_info(
      linear_reg() |> set_engine("glmnet")
    )
  })
})

test_that("tunable parameters for linear_reg + stan", {
  expect_snapshot({
    display_tunable_call_info(
      linear_reg() |> set_engine("stan")
    )
  })
})

test_that("tunable parameters for linear_reg + spark", {
  expect_snapshot({
    display_tunable_call_info(
      linear_reg() |> set_engine("spark")
    )
  })
})

test_that("tunable parameters for linear_reg + keras", {
  expect_snapshot({
    display_tunable_call_info(
      linear_reg() |> set_engine("keras")
    )
  })
})

test_that("tunable parameters for linear_reg + brulee", {
  expect_snapshot({
    display_tunable_call_info(
      linear_reg() |> set_engine("brulee")
    )
  })
})

test_that("tunable parameters for linear_reg + quantreg", {
  expect_snapshot({
    display_tunable_call_info(
      linear_reg() |> set_engine("quantreg")
    )
  })
})

test_that("tunable parameters for linear_reg + gee", {
  expect_snapshot({
    display_tunable_call_info(
      linear_reg() |> set_engine("gee")
    )
  })
})

test_that("tunable parameters for linear_reg + lme", {
  expect_snapshot({
    display_tunable_call_info(
      linear_reg() |> set_engine("lme")
    )
  })
})

test_that("tunable parameters for linear_reg + lmer", {
  expect_snapshot({
    display_tunable_call_info(
      linear_reg() |> set_engine("lmer")
    )
  })
})

test_that("tunable parameters for linear_reg + glmer", {
  expect_snapshot({
    display_tunable_call_info(
      linear_reg() |> set_engine("glmer")
    )
  })
})

test_that("tunable parameters for linear_reg + gls", {
  expect_snapshot({
    display_tunable_call_info(
      linear_reg() |> set_engine("gls")
    )
  })
})

test_that("tunable parameters for linear_reg + stan_glmer", {
  expect_snapshot({
    display_tunable_call_info(
      linear_reg() |> set_engine("stan_glmer")
    )
  })
})
