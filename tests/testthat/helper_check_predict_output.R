

check_predict_output <- function(
  predictions,
  passed_data,
  type) {

  expect_true(is_tibble(predictions))

  ## what should be in the returned prediction tibble

  passed_cols <- colnames(passed_data)
  pred_cols <- colnames(predictions)
  pred_classes <- sapply(predictions, class)

  expect_false(
    any(passed_cols %in% pred_cols),
    info = "Columns from `new_data` must not appear in prediction tibble."
  )

  expect_equal(
    nrow(predictions), nrow(passed_data),
    info = "Prediction tibble must have same number of rows as `new_data`."
  )

  ## column presence

  expect_.pred_column <- c("response", "class", "link", "conf_int", "pred_int")
  expect_.pred_level_columns <- "prob"
  expect_.pred_interval_cols <- c("conf_int", "pred_int")

  if (type %in% expect_.pred_column)
    expect_true(".pred" %in% pred_cols)

  if (type %in% expect_.pred_level_columns)
    expect_true(all(stringr::str_detect(pred_cols, ".pred_")))

  if (type %in% expect_.pred_interval_cols)
    expect_true(all(c(".pred_lower", ".pred_upper") %in% pred_cols))

  ## column type

  expect_numeric <- c("response", "prob", "link", "conf_int", "pred_int")
  expect_factor <- "class"

  if (type %in% expect_numeric)
    expect_true(all(pred_classes == "numeric"))

  if (type %in% expect_factor)
    expect_true(all(pred_classes == "factor"))


  ## check for presence of interval attributes
  if (type == "conf_int")
    expect_equal(attr(predictions, "interval"), "confidence")

  if (type == "pred_int")
    expect_equal(attr(predictions, "interval"), "prediction")

  if (type %in% c("conf_int", "pred_int"))
    expect_false(is.null(attr(predictions, "level")))

  ## class probabilities must add to one
  if (type == "class")
    expect_equivalent(rowSums(predictions), 1)
}
