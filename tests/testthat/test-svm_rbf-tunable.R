test_that("tunable parameters for svm_rbf + kernlab", {
  expect_snapshot({
    display_tunable_call_info(
      svm_rbf() |> set_engine("kernlab")
    )
  })
})

test_that("tunable parameters for svm_rbf + liquidSVM", {
  expect_snapshot({
    display_tunable_call_info(
      svm_rbf() |> set_engine("liquidSVM")
    )
  })
})
