test_that("probability truncation via trunc_probs()", {
  probs <- seq(0, 1, length.out = 5)

  probs_trunc_001 <- parsnip:::trunc_probs(probs, trunc = 0.01)
  expect_equal(probs_trunc_001[1], 0.01)
  expect_equal(probs_trunc_001[2:5], probs[2:5])

  probs_trunc_04 <- parsnip:::trunc_probs(probs, trunc = 0.4)
  data_derived_trunc <- min(probs[probs > 0]) / 2
  expect_equal(probs_trunc_04[1], data_derived_trunc)
  expect_equal(probs_trunc_04[2:5], probs[2:5])

  probs_trunc_04_na <- parsnip:::trunc_probs(c(NA, probs), 0.4)
  expect_identical(probs_trunc_04_na[1], NA_real_)
  expect_equal(probs_trunc_04_na[2], data_derived_trunc)
  expect_equal(probs_trunc_04_na[3:6], probs[2:5])

  probs <- (1:200)/200
  expect_identical(
    parsnip:::trunc_probs(probs, trunc = 0.01),
    probs
  )
})

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

test_that("no names in weight values", {
  # See #1023

  surv_obj <-
    structure(
      c(9, 13, 13, 18, 23, 28, 1, 1, 0, 1, 1, 0),
      dim = c(6L, 2L),
      dimnames = list(NULL, c("time", "status")),
      type = "right",
      class = "Surv"
    )

  row_1 <- parsnip:::graf_weight_time_vec(surv_obj[1,,drop = FALSE], 1.0)
  row_5 <- parsnip:::graf_weight_time_vec(surv_obj, 1.0)
  expect_null(names(row_1))
  expect_null(names(row_5))
})
