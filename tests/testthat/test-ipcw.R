test_that('probability truncation', {
  probs <- seq(0, 1, length.out = 5)

  expect_equal(
    min(parsnip:::trunc_probs(probs, .4)),
    min(probs[probs > 0]) / 2
  )
  expect_equal(
    min(parsnip:::trunc_probs(c(NA, probs), .4), na.rm = TRUE),
    min(probs[probs > 0]) / 2
  )
  expect_equal(
    min(parsnip:::trunc_probs(probs)),
    0.01
  )
  expect_equal(
    min(parsnip:::trunc_probs((1:200)/200)),
    1 / 200
  )

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
  expect_snapshot(error = TRUE, parsnip:::.filter_eval_time(-1))
  expect_null(parsnip:::.filter_eval_time(NULL))
})
