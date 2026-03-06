test_that("tunable parameters for svm_linear + LiblineaR", {
  expect_snapshot(
    display_tunable_call_info(
      svm_linear() |> set_engine("LiblineaR")
    )
  )
})

test_that("tunable parameters for svm_linear + kernlab", {
  expect_snapshot(
    display_tunable_call_info(
      svm_linear() |> set_engine("kernlab")
    )
  )
})
