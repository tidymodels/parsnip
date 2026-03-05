test_that("tunable parameters for null_model + parsnip", {
  expect_snapshot({
    display_tunable_call_info(
      null_model() |> set_engine("parsnip")
    )
  })
})
