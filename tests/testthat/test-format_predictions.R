test_that("format_predictions() handles numeric type", {
  x <- c(1.5, 2.5, 3.5)
  res <- format_predictions(x, "numeric")

  expect_s3_class(res, "tbl_df")
  expect_named(res, ".pred")
  expect_equal(res$.pred, x)
})

test_that("format_predictions() handles class type", {
  x <- factor(c("a", "b", "a"))
  res <- format_predictions(x, "class")

  expect_s3_class(res, "tbl_df")
  expect_named(res, ".pred_class")
  expect_equal(res$.pred_class, x)
})

test_that("format_predictions() handles prob type", {
  x <- data.frame(a = c(0.2, 0.3), b = c(0.8, 0.7))
  res <- format_predictions(x, "prob")

  expect_s3_class(res, "tbl_df")
  expect_named(res, c(".pred_a", ".pred_b"))
  expect_equal(res$.pred_a, x$a)
  expect_equal(res$.pred_b, x$b)
})

test_that("format_predictions() handles time type", {
  x <- c(10, 20, 30)
  res <- format_predictions(x, "time")

  expect_s3_class(res, "tbl_df")
  expect_named(res, ".pred_time")
  expect_equal(res$.pred_time, x)
})

test_that("format_predictions() handles survival type", {
  x <- c(0.9, 0.8, 0.7)
  res <- format_predictions(x, "survival")

  expect_s3_class(res, "tbl_df")
  expect_named(res, ".pred")
  expect_equal(res$.pred, x)
})

test_that("format_predictions() handles linear_pred type", {
  x <- c(-1, 0, 1)
  res <- format_predictions(x, "linear_pred")

  expect_s3_class(res, "tbl_df")
  expect_named(res, ".pred_linear_pred")
  expect_equal(res$.pred_linear_pred, x)
})

test_that("format_predictions() handles hazard type", {
  x <- c(0.1, 0.2, 0.3)
  res <- format_predictions(x, "hazard")

  expect_s3_class(res, "tbl_df")
  expect_named(res, ".pred")
  expect_equal(res$.pred, x)
})

test_that("format_predictions() returns unknown types unchanged", {
  x <- data.frame(a = 1:3)
  res <- format_predictions(x, "unknown_type")

  expect_equal(res, x)
})

test_that("format_predictions() matches individual format functions", {
  x_vec <- c(1.5, 2.5, 3.5)

  expect_equal(
    format_predictions(x_vec, "numeric"),
    format_num(x_vec)
  )
  expect_equal(
    format_predictions(x_vec, "time"),
    format_time(x_vec)
  )
  expect_equal(
    format_predictions(x_vec, "survival"),
    format_survival(x_vec)
  )
  expect_equal(
    format_predictions(x_vec, "linear_pred"),
    format_linear_pred(x_vec)
  )
  expect_equal(
    format_predictions(x_vec, "hazard"),
    format_hazard(x_vec)
  )

  x_factor <- factor(c("a", "b", "a"))
  expect_equal(
    format_predictions(x_factor, "class"),
    format_class(x_factor)
  )

  x_df <- data.frame(a = c(0.2, 0.3), b = c(0.8, 0.7))
  expect_equal(
    format_predictions(x_df, "prob"),
    format_classprobs(x_df)
  )
})
