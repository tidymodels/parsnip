test_that("parsnip_quantiles error types", {
  expect_snapshot(
    error = TRUE,
    parsnip_quantiles(1:10, 1:4 / 5)
  )
  expect_snapshot(
    error = TRUE,
    parsnip_quantiles(matrix(1:20, 5), -1:4 / 5)
  )
  expect_snapshot(
    error = TRUE,
    parsnip_quantiles(matrix(1:20, 5), 1:5 / 6)
  )
  expect_snapshot(
    error = TRUE,
    parsnip_quantiles(matrix(1:20, 5), 4:1 / 5)
  )
})

test_that("parsnip_quantiles outputs", {
  v <- parsnip_quantiles(matrix(1:20, 5), 1:4 / 5)
  expect_s3_class(v, "parsnip_quantiles")
  expect_identical(attr(v, "quantile_levels"), 1:4 / 5)
  expect_identical(
    vctrs::vec_data(v),
    lapply(vctrs::vec_chop(matrix(1:20, 5)), drop)
  )
})

test_that("parsnip_quantiles formatting", {
  v <- parsnip_quantiles(matrix(1:20, 5), 1:4 / 5)
  expect_snapshot(print(v))
  expect_snapshot(print(parsnip_quantiles(matrix(1:18, 9), c(1/3, 2/3))))
  expect_snapshot(print(
    parsnip_quantiles(matrix(seq(0.01, 1 - 0.01, length.out = 6), 3), c(.2, .8))
  ))
  expect_snapshot(print(tibble(qntls = v)))
  m <- matrix(1:20, 5)
  m[2, 3] <- NA
  m[4, 2] <- NA
  expect_snapshot(print(parsnip_quantiles(m, 1:4 / 5)))

})
