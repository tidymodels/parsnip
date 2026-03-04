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

test_that("format_predictions() handles raw type", {
  x <- list(a = 1, b = 2)
  res <- format_predictions(x, "raw")

  expect_equal(res, x)
})

test_that("format_predictions() handles conf_int type", {
  x <- data.frame(.pred_lower = c(1, 2), .pred_upper = c(3, 4))
  res <- format_predictions(x, "conf_int")

  expect_s3_class(res, "tbl_df")
  expect_named(res, c(".pred_lower", ".pred_upper"))
  expect_equal(res$.pred_lower, c(1, 2))
  expect_equal(res$.pred_upper, c(3, 4))
})

test_that("format_predictions() handles pred_int type", {
  x <- data.frame(.pred_lower = c(0, 1), .pred_upper = c(2, 3))
  res <- format_predictions(x, "pred_int")

  expect_s3_class(res, "tbl_df")
  expect_named(res, c(".pred_lower", ".pred_upper"))
  expect_equal(res$.pred_lower, c(0, 1))
  expect_equal(res$.pred_upper, c(2, 3))
})

test_that("format_predictions() handles quantile type", {
  x <- tibble::tibble(
    .pred_quantile = hardhat::quantile_pred(
      matrix(1:6, nrow = 2),
      c(0.25, 0.5, 0.75)
    )
  )
  res <- format_predictions(x, "quantile")

  expect_s3_class(res, "tbl_df")
  expect_named(res, ".pred_quantile")
  expect_s3_class(res$.pred_quantile, "quantile_pred")
})

test_that("format_predictions() returns unknown types unchanged", {
  x <- data.frame(a = 1:3)
  res <- format_predictions(x, "unknown_type")

  expect_equal(res, x)
})

test_that("format_predictions() matches individual format functions", {
  withr::local_options(lifecycle_verbosity = "quiet")

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

# Deprecation tests
test_that("format_num() is deprecated", {
  expect_snapshot(. <- format_num(c(1, 2, 3)))
})

test_that("format_class() is deprecated", {
  expect_snapshot(. <- format_class(factor(c("a", "b"))))
})

test_that("format_classprobs() is deprecated", {
  expect_snapshot(. <- format_classprobs(data.frame(a = 0.5, b = 0.5)))
})

test_that("format_time() is deprecated", {
  expect_snapshot(. <- format_time(c(1, 2, 3)))
})

test_that("format_survival() is deprecated", {
  expect_snapshot(. <- format_survival(c(0.9, 0.8)))
})

test_that("format_linear_pred() is deprecated", {
  expect_snapshot(. <- format_linear_pred(c(-1, 0, 1)))
})

test_that("format_hazard() is deprecated", {
  expect_snapshot(. <- format_hazard(c(0.1, 0.2)))
})
