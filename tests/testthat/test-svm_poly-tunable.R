test_that("tunable parameters for svm_poly + kernlab", {
  expect_snapshot(
    display_tunable_call_info(
      svm_poly() |> set_engine("kernlab")
    )
  )
})
