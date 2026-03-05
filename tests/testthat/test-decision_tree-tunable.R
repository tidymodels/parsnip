test_that("tunable parameters for decision_tree + rpart", {
  expect_snapshot({
    display_tunable_call_info(
      decision_tree() |> set_engine("rpart")
    )
  })
})

test_that("tunable parameters for decision_tree + C5.0", {
  expect_snapshot({
    display_tunable_call_info(
      decision_tree() |> set_engine("C5.0")
    )
  })
})

test_that("tunable parameters for decision_tree + spark", {
  expect_snapshot({
    display_tunable_call_info(
      decision_tree() |> set_engine("spark")
    )
  })
})

test_that("tunable parameters for decision_tree + partykit", {
  expect_snapshot({
    display_tunable_call_info(
      decision_tree() |> set_engine("partykit")
    )
  })
})
