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
})


test_that('time filtering', {
  times_1 <- 0:10
  times_2 <- c(Inf, NA, -3, times_1, times_1)

  expect_equal(
    parsnip:::filter_eval_time(times_1),
    times_1
  )
  expect_equal(
    parsnip:::filter_eval_time(times_1),
    times_1
  )
  expect_snapshot_error(parsnip:::filter_eval_time(-1))
})





