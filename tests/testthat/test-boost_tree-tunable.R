test_that("tunable parameters for boost_tree + xgboost", {
  expect_snapshot({
    display_tunable_call_info(
      boost_tree() |> set_engine("xgboost")
    )
  })
})

test_that("tunable parameters for boost_tree + C5.0", {
  expect_snapshot({
    display_tunable_call_info(
      boost_tree() |> set_engine("C5.0")
    )
  })
})

test_that("tunable parameters for boost_tree + spark", {
  expect_snapshot({
    display_tunable_call_info(
      boost_tree() |> set_engine("spark")
    )
  })
})

test_that("tunable parameters for boost_tree + lightgbm", {
  expect_snapshot({
    display_tunable_call_info(
      boost_tree() |> set_engine("lightgbm")
    )
  })
})

test_that("tunable parameters for boost_tree + catboost", {
  expect_snapshot({
    display_tunable_call_info(
      boost_tree() |> set_engine("catboost")
    )
  })
})
