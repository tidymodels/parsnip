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

test_that("trunc_probs()", {
  probs_1 <- (0:10) / 20
  probs_2 <- probs_1
  probs_2[3] <- NA_real_

  expect_equal(parsnip:::trunc_probs(probs_1, 0), probs_1)
  expect_equal(parsnip:::trunc_probs(probs_2, 0), probs_2)
  expect_equal(
    parsnip:::trunc_probs(probs_1, 0.1),
    ifelse(probs_1 < 0.05 / 2, 0.05 / 2, probs_1)
  )
  expect_equal(min(parsnip:::trunc_probs(probs_2, 0.1), na.rm = TRUE), 0.05 / 2)
  expect_equal(is.na(parsnip:::trunc_probs(probs_2, 0.1)),is.na(probs_2))
})

test_that('time filtering', {
  times_1 <- 0:10
  times_2 <- c(Inf, NA, -3, times_1, times_1)
  times_3 <- c(10, 1:9)

  expect_equal(
    parsnip:::.filter_eval_time(times_1),
    times_1
  )
  expect_equal(
    parsnip:::.filter_eval_time(times_1),
    times_1
  )
  expect_equal(
    parsnip:::.filter_eval_time(times_3),
    times_3
  )
  expect_snapshot_warning(parsnip:::.filter_eval_time(times_2))
  expect_snapshot_warning(parsnip:::.filter_eval_time(times_2[3:4]))
  expect_snapshot(error = TRUE, parsnip:::.filter_eval_time(-1))
  expect_null(parsnip:::.filter_eval_time(NULL))
})
