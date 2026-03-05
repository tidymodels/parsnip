test_that("tunable parameters for nearest_neighbor + kknn", {
  expect_snapshot({
    display_tunable_call_info(
      nearest_neighbor() |> set_engine("kknn")
    )
  })
})
