test_that("probability truncation via trunc_probs()", {
  probs <- seq(0, 1, length.out = 5)

  probs_trunc_001 <- trunc_probs(probs, trunc = 0.01)
  expect_equal(probs_trunc_001[1], 0.01)
  expect_equal(probs_trunc_001[2:5], probs[2:5])

  probs_trunc_04 <- trunc_probs(probs, trunc = 0.4)
  data_derived_trunc <- min(probs[probs > 0]) / 2
  expect_equal(probs_trunc_04[1], data_derived_trunc)
  expect_equal(probs_trunc_04[2:5], probs[2:5])

  probs_trunc_04_na <- trunc_probs(c(NA, probs), 0.4)
  expect_identical(probs_trunc_04_na[1], NA_real_)
  expect_equal(probs_trunc_04_na[2], data_derived_trunc)
  expect_equal(probs_trunc_04_na[3:6], probs[2:5])

  probs <- (1:200) / 200
  expect_identical(
    trunc_probs(probs, trunc = 0.01),
    probs
  )
})

test_that("`predict_survival()` errors early when `add_censoring_weights = TRUE` but `new_data` has no Surv column", {
  fake_fit <- list(
    spec = list(
      mode = "censored regression",
      method = list(pred = list(survival = list()))
    ),
    fit = NULL
  )
  class(fake_fit) <- "model_fit"
  no_surv_data <- tibble::tibble(x = 1)

  expect_snapshot(
    error = TRUE,
    predict_survival(
      fake_fit,
      new_data = no_surv_data,
      eval_time = 1,
      add_censoring_weights = TRUE
    )
  )
})
