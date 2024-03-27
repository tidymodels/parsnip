test_that("extract_fit_time() works", {
  lm_fit <- linear_reg() %>% fit(mpg ~ ., data = mtcars)

  res <- extract_fit_time(lm_fit)

  expect_true(is_tibble(res))
  expect_identical(names(res), c("process_id", "elapsed"))
  expect_identical(res$process_id, "linear_reg")
  expect_true(is.double(res$elapsed))
  expect_true(res$elapsed >= 0)

  expect_snapshot(
    error = TRUE,
    extract_fit_time(lm_fit, summarize = FALSE)
  )

  lm_fit$elapsed$elapsed <- NULL

  expect_snapshot(
    error = TRUE,
    extract_fit_time(lm_fit)
  )
})
