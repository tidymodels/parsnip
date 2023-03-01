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
    parsnip:::.filter_eval_time(times_1),
    times_1
  )
  expect_equal(
    parsnip:::.filter_eval_time(times_1),
    times_1
  )
  expect_snapshot(error = TRUE, parsnip:::.filter_eval_time(-1))
  expect_null(parsnip:::.filter_eval_time(NULL))
})

test_that('probability truncation', {
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

test_that('calcuate evaluation time', {

  library(survival)

  times <- 1:10
  cens <- rep(0:1, times = 5)

  surv_obj <- Surv(times, cens)

  eval_0 <- parsnip:::graf_weight_time(surv_obj, eval_time = 0)
  eval_05 <- parsnip:::graf_weight_time(surv_obj, eval_time = 5, eps = 1)
  eval_11 <- parsnip:::graf_weight_time(surv_obj, eval_time = 11, rows = 11:20, eps = 0)

  na_05 <- is.na(eval_05$weight_time)
  na_11 <- is.na(eval_11$weight_time)

  expect_equal(eval_0$weight_time, rep(0, 10))
  expect_equal(eval_0$.row, 1:10)

  expect_equal(
    which(na_05),
    which(times <= 5 & cens == 0)
  )
  expect_equal(
    eval_05$weight_time[!na_05],
    ifelse(times[!na_05] - 1 < 5, times[!na_05] - 1, 4)
  )

  expect_equal(
    which(na_11),
    which(cens == 0)
  )
  expect_equal(
    eval_11$weight_time[!na_11],
    (1:5) * 2
  )
  expect_equal(eval_11$.row, 11:20)

})

test_that('compute Graf weights', {

  library(parsnip)
  library(survival)
  library(censored)
  library(workflows)
  library(dplyr)

  times <- 1:10
  cens <- c(0, rep(1, 9))
  surv_obj <- Surv(times, cens)
  df <- data.frame(surv = surv_obj, x = -1:8)
  fit <- survival_reg() %>% fit(surv ~ x, data = df)
  wflow_fit <-
    workflow() %>%
    add_model(survival_reg(), formula = surv ~ x) %>%
    add_variables(surv, x) %>%
    fit(data = df)

  eval_0 <- parsnip:::graf_weight_time(surv_obj, eval_time = 0)
  eval_05 <- parsnip:::graf_weight_time(surv_obj, eval_time = 5, eps = 1)
  eval_11 <- parsnip:::graf_weight_time(surv_obj, eval_time = 11, rows = 11:20, eps = 0)

  cens_prob_00 <- predict(fit$censor_probs, time = eval_0$weight_time, as_vector = TRUE)
  cens_prob_05 <- predict(fit$censor_probs, time = eval_05$weight_time, as_vector = TRUE)
  cens_prob_11 <- predict(fit$censor_probs, time = eval_11$weight_time, as_vector = TRUE)

  wts_00 <- .censoring_weights_graf(fit, df, 0)
  wts_05 <- .censoring_weights_graf(fit, df, 5)
  wts_11 <- .censoring_weights_graf(fit, df, 11)

  wflow_wts_00 <- .censoring_weights_graf(wflow_fit, df, 0)
  wflow_wts_05 <- .censoring_weights_graf(wflow_fit, df, 5)
  wflow_wts_11 <- .censoring_weights_graf(wflow_fit, df, 11)

  expect_equal(1 / cens_prob_00, wts_00$.weight_cens)
  expect_equal(1 / cens_prob_05, wts_05$.weight_cens)
  expect_equal(1 / cens_prob_11, wts_11$.weight_cens)

  expect_equal(1 / cens_prob_00, wflow_wts_00$.weight_cens)
  expect_equal(1 / cens_prob_05, wflow_wts_05$.weight_cens)
  expect_equal(1 / cens_prob_11, wflow_wts_11$.weight_cens)

  expect_true(inherits(wts_00, "data.frame"))
  expect_equal(names(wts_00), c(".row", "eval_time", ".prob_cens", ".weight_cens"))
  expect_equal(nrow(wts_00), nrow(df))

  expect_snapshot(.censoring_weights_graf(2, df, 0), error = TRUE)

})

