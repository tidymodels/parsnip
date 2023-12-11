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
