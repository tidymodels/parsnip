test_that("tunable parameters for bart + dbarts", {
  expect_snapshot({
    display_tunable_call_info(
      bart() |> set_engine("dbarts")
    )
  })
})
