test_that(".filter_eval_time()", {
  times_basic <- 0:10
  expect_equal(
    parsnip:::.filter_eval_time(times_basic),
    times_basic
  )

  times_dont_reorder <- c(10, 1:9)
  expect_equal(
    parsnip:::.filter_eval_time(times_dont_reorder),
    times_dont_reorder
  )

  expect_null(parsnip:::.filter_eval_time(NULL))

  times_duplicated <- c(times_basic, times_basic)
  expect_snapshot(
    parsnip:::.filter_eval_time(times_duplicated)
  )

  expect_snapshot(error = TRUE, parsnip:::.filter_eval_time(-1))

  times_remove_plural <- c(Inf, NA, -3, times_basic)
  expect_snapshot(parsnip:::.filter_eval_time(times_remove_plural))

  times_remove_singular <- c(-3, times_basic)
  expect_snapshot(parsnip:::.filter_eval_time(times_remove_singular))
})
