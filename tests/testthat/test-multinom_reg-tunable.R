test_that("tunable parameters for multinom_reg + glmnet", {
  expect_snapshot({
    display_tunable_call_info(
      multinom_reg() |> set_engine("glmnet")
    )
  })
})

test_that("tunable parameters for multinom_reg + spark", {
  expect_snapshot({
    display_tunable_call_info(
      multinom_reg() |> set_engine("spark")
    )
  })
})

test_that("tunable parameters for multinom_reg + keras", {
  expect_snapshot({
    display_tunable_call_info(
      multinom_reg() |> set_engine("keras")
    )
  })
})

test_that("tunable parameters for multinom_reg + nnet", {
  expect_snapshot({
    display_tunable_call_info(
      multinom_reg() |> set_engine("nnet")
    )
  })
})

test_that("tunable parameters for multinom_reg + brulee", {
  expect_snapshot({
    display_tunable_call_info(
      multinom_reg() |> set_engine("brulee")
    )
  })
})
