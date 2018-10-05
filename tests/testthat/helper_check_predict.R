check_predict_basic <- function(
  predictions,
  passed_data) {

  expect_true(tibble::is_tibble(predictions))

  expect_equal(
    nrow(predictions), nrow(passed_data),
    info = "Prediction tibble must have same number of rows as `new_data`."
  )
}
