test_that("tunable parameters for rand_forest + ranger", {
  expect_snapshot(
    display_tunable_call_info(
      rand_forest() |> set_engine("ranger")
    )
  )
})

test_that("tunable parameters for rand_forest + randomForest", {
  expect_snapshot(
    display_tunable_call_info(
      rand_forest() |> set_engine("randomForest")
    )
  )
})

test_that("tunable parameters for rand_forest + spark", {
  expect_snapshot(
    display_tunable_call_info(
      rand_forest() |> set_engine("spark")
    )
  )
})

test_that("tunable parameters for rand_forest + partykit", {
  expect_snapshot(
    display_tunable_call_info(
      rand_forest() |> set_engine("partykit")
    )
  )
})

test_that("tunable parameters for rand_forest + aorsf", {
  expect_snapshot(
    display_tunable_call_info(
      rand_forest() |> set_engine("aorsf")
    )
  )
})
