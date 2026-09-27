test_that(".filter_eval_time()", {
  times_basic <- 0:10
  expect_equal(
    .filter_eval_time(times_basic),
    times_basic
  )

  times_dont_reorder <- c(10, 1:9)
  expect_equal(
    .filter_eval_time(times_dont_reorder),
    times_dont_reorder
  )

  expect_null(.filter_eval_time(NULL))

  times_duplicated <- c(times_basic, times_basic)
  expect_snapshot(
    .filter_eval_time(times_duplicated)
  )

  expect_snapshot(error = TRUE, .filter_eval_time(-1))

  times_remove_plural <- c(Inf, NA, -3, times_basic)
  expect_snapshot(.filter_eval_time(times_remove_plural))

  times_remove_singular <- c(-3, times_basic)
  expect_snapshot(.filter_eval_time(times_remove_singular))
})

test_that("exported survival helpers match their documented usage", {
  # `.extract_surv_time()` and `.extract_surv_status()` are re-exported from
  # the standalone file with `assign()`, so roxygen cannot derive `\usage` and
  # `R/survival-helpers.R` states it by hand. If a signature changes upstream,
  # this fails as a reminder to update the `@usage` tag with it.
  expect_named(formals(.extract_surv_time), "surv")
  expect_named(formals(.extract_surv_status), "surv")
})
