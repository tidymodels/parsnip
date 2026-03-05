test_that("tunable parameters for mlp + keras", {
  expect_snapshot({
    display_tunable_call_info(
      mlp() |> set_engine("keras")
    )
  })
})

test_that("tunable parameters for mlp + nnet", {
  expect_snapshot({
    display_tunable_call_info(
      mlp() |> set_engine("nnet")
    )
  })
})

test_that("tunable parameters for mlp + brulee", {
  expect_snapshot({
    display_tunable_call_info(
      mlp() |> set_engine("brulee")
    )
  })
})

test_that("tunable parameters for mlp + brulee_two_layer", {
  expect_snapshot({
    display_tunable_call_info(
      mlp() |> set_engine("brulee_two_layer")
    )
  })
})
