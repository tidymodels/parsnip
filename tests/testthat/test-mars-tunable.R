test_that("tunable parameters for mars + earth", {
  expect_snapshot({
    display_tunable_call_info(
      mars() |> set_engine("earth")
    )
  })
})
