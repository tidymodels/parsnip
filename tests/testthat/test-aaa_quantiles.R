test_that("vec_quantiles error types", {
  expect_snapshot(
    error = TRUE,
    vec_quantiles(1:10, 1:4 / 5)
  )
  expect_snapshot(
    error = TRUE,
    vec_quantiles(matrix(1:20, 5), -1:4 / 5)
  )
  expect_snapshot(
    error = TRUE,
    vec_quantiles(matrix(1:20, 5), 1:5 / 6)
  )
  expect_snapshot(
    error = TRUE,
    vec_quantiles(matrix(1:20, 5), 4:1 / 5)
  )
})

test_that("vec_quantiles outputs", {
  v <- vec_quantiles(matrix(1:20, 5), 1:4 / 5)
  expect_s3_class(v, "vctrs_quantiles")
  expect_identical(attr(v, "quantile_levels"), 1:4 / 5)
  expect_identical(
    vctrs::vec_data(v),
    lapply(vctrs::vec_chop(matrix(1:20, 5)), drop)
  )
})
