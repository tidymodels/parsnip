test_that("format_predictions() handles numeric type", {
  x <- c(1.5, 2.5, 3.5)
  exp <- tibble::tibble(.pred = x)

  expect_identical(format_predictions(x, "numeric"), exp)
})

test_that("format_predictions() handles class type", {
  x <- factor(c("a", "b", "a"))
  exp <- tibble::tibble(.pred_class = x)

  expect_identical(format_predictions(x, "class"), exp)
})

test_that("format_predictions() handles prob type", {
  x <- data.frame(a = c(0.2, 0.3), b = c(0.8, 0.7))
  exp <- tibble::tibble(.pred_a = c(0.2, 0.3), .pred_b = c(0.8, 0.7))

  expect_identical(format_predictions(x, "prob"), exp)
})

test_that("format_predictions() handles time type", {
  x <- c(10, 20, 30)
  exp <- tibble::tibble(.pred_time = x)

  expect_identical(format_predictions(x, "time"), exp)
})

test_that("format_predictions() handles survival type", {
  x <- c(0.9, 0.8, 0.7)
  exp <- tibble::tibble(.pred = x)

  expect_identical(format_predictions(x, "survival"), exp)
})

test_that("format_predictions() handles linear_pred type", {
  x <- c(-1, 0, 1)
  exp <- tibble::tibble(.pred_linear_pred = x)

  expect_identical(format_predictions(x, "linear_pred"), exp)
})

test_that("format_predictions() handles hazard type", {
  x <- c(0.1, 0.2, 0.3)
  exp <- tibble::tibble(.pred = x)

  expect_identical(format_predictions(x, "hazard"), exp)
})

test_that("format_predictions() handles raw type", {
  x <- list(a = 1, b = 2)

  expect_identical(format_predictions(x, "raw"), x)
})

test_that("format_predictions() handles conf_int type", {
  x <- data.frame(.pred_lower = c(1, 2), .pred_upper = c(3, 4))
  exp <- tibble::tibble(.pred_lower = c(1, 2), .pred_upper = c(3, 4))

  expect_identical(format_predictions(x, "conf_int"), exp)
})

test_that("format_predictions() handles pred_int type", {
  x <- data.frame(.pred_lower = c(0, 1), .pred_upper = c(2, 3))
  exp <- tibble::tibble(.pred_lower = c(0, 1), .pred_upper = c(2, 3))

  expect_identical(format_predictions(x, "pred_int"), exp)
})

test_that("format_predictions() handles quantile type", {
  x <- tibble::tibble(
    .pred_quantile = hardhat::quantile_pred(
      matrix(1:6, nrow = 2),
      c(0.25, 0.5, 0.75)
    )
  )

  expect_identical(format_predictions(x, "quantile"), x)
})

test_that("format_predictions() returns unknown types unchanged", {
  x <- data.frame(a = 1:3)
  res <- format_predictions(x, "unknown_type")

  expect_identical(res, x)
})

test_that("format_predictions() matches individual format functions", {
  withr::local_options(lifecycle_verbosity = "quiet")

  x_vec <- c(1.5, 2.5, 3.5)

  expect_identical(
    format_predictions(x_vec, "numeric"),
    format_num(x_vec)
  )
  expect_identical(
    format_predictions(x_vec, "time"),
    format_time(x_vec)
  )
  expect_identical(
    format_predictions(x_vec, "survival"),
    format_survival(x_vec)
  )
  expect_identical(
    format_predictions(x_vec, "linear_pred"),
    format_linear_pred(x_vec)
  )
  expect_identical(
    format_predictions(x_vec, "hazard"),
    format_hazard(x_vec)
  )

  x_factor <- factor(c("a", "b", "a"))
  expect_identical(
    format_predictions(x_factor, "class"),
    format_class(x_factor)
  )

  x_df <- data.frame(a = c(0.2, 0.3), b = c(0.8, 0.7))
  expect_identical(
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
