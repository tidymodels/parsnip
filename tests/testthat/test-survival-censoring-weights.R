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

test_that(".get_surv() returns the column when y_var has one name and column is a Surv", {
  surv <- survival::Surv(c(1, 2, 3), c(1, 0, 1))
  fake_fit <- list(preproc = list(y_var = "outcome"))
  new_data <- tibble::tibble(outcome = surv, x = 1:3)

  expect_equal(.get_surv(fake_fit, new_data), surv)
})

test_that(".get_surv() errors when y_var names a column that is not a Surv", {
  fake_fit <- list(preproc = list(y_var = "outcome"))
  new_data <- tibble::tibble(outcome = 1:3, x = 4:6)

  expect_snapshot(error = TRUE, .get_surv(fake_fit, new_data))
  expect_null(.get_surv(fake_fit, new_data, fail = FALSE))
})

test_that(".get_surv() errors when the y_var column is missing from new_data", {
  fake_fit <- list(preproc = list(y_var = "outcome"))
  new_data <- tibble::tibble(x = 1:3)

  expect_snapshot(error = TRUE, .get_surv(fake_fit, new_data))
  expect_null(.get_surv(fake_fit, new_data, fail = FALSE))
})

test_that(".get_surv() builds a Surv from two y_var columns", {
  fake_fit <- list(preproc = list(y_var = c("time", "status")))
  new_data <- tibble::tibble(time = c(1, 2, 3), status = c(1, 0, 1), x = 4:6)

  expect_equal(
    .get_surv(fake_fit, new_data),
    survival::Surv(c(1, 2, 3), c(1, 0, 1))
  )
})

test_that(".get_surv() errors when y_var has two names but columns are missing", {
  fake_fit <- list(preproc = list(y_var = c("time", "status")))
  new_data <- tibble::tibble(x = 1:3)

  expect_snapshot(error = TRUE, .get_surv(fake_fit, new_data))
  expect_null(.get_surv(fake_fit, new_data, fail = FALSE))
})

test_that(".get_surv() errors when y_var has more than two names", {
  fake_fit <- list(preproc = list(y_var = c("a", "b", "c")))
  new_data <- tibble::tibble(a = 1:3, b = 1:3, c = 1:3)

  expect_snapshot(error = TRUE, .get_surv(fake_fit, new_data))
  expect_null(.get_surv(fake_fit, new_data, fail = FALSE))
})

test_that(".get_surv() scans new_data for a Surv column when y_var is empty", {
  surv <- survival::Surv(c(1, 2, 3), c(1, 0, 1))
  fake_fit <- list(preproc = list(y_var = character(0)))
  new_data <- tibble::tibble(outcome = surv, x = 1:3)

  expect_equal(.get_surv(fake_fit, new_data), surv)
})
