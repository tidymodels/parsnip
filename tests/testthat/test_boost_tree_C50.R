library(testthat)
context("boosted tree execution with C5.0")
library(parsnip)
library(tibble)

###################################################################

ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

###################################################################

data("lending_club")
lending_club <- head(lending_club, 200)
num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")
lc_basic <- boost_tree(mode = "classification")

test_that('C5.0 execution', {

  skip_if_not_installed("C50")

  # passes interactively but not on R CMD check
  expect_error(
    res <- fit(
      lc_basic,
      Class ~ log(funded_amnt) + int_rate,
      data = lending_club,
      control = ctrl,
      engine = "C5.0"
    ),
    regexp = NA
  )
  expect_error(
    res <- fit_xy(
      lc_basic,
      x = lending_club[, num_pred],
      y = lending_club$Class,
      engine = "C5.0",
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- fit(
      lc_basic,
      funded_amnt ~ term,
      data = lending_club,
      engine = "C5.0",
      control = ctrl
    )
  )

  # passes interactively but not on R CMD check
  C5.0_form_catch <- fit(
    lc_basic,
    funded_amnt ~ term,
    data = lending_club,
    engine = "C5.0",
    control = caught_ctrl
  )
  expect_true(inherits(C5.0_form_catch$fit, "try-error"))

  C5.0_xy_catch <- fit_xy(
    lc_basic,
    engine = "C5.0",
    control = caught_ctrl,
    x = lending_club[, num_pred],
    y = lending_club$total_bal_il
  )
  expect_true(inherits(C5.0_xy_catch$fit, "try-error"))
})

test_that("C5.0 prediction", {

  skip_if_not_installed("C50")

  form_fit <- fit(
    lc_basic,
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    engine = "C5.0"
  )

  expect_silent(form_pred <- predict(form_fit, lending_club))
  check_predict_basic(form_pred, lending_club)

  xy_fit <- fit_xy(
    lc_basic,
    x = lending_club[, num_pred],
    y = lending_club$Class,
    engine = "C5.0",
    control = ctrl
  )

  passed_data <- lending_club[, num_pred]
  expect_silent(xy_pred <- predict(xy_fit, passed_data))
  check_predict_basic(xy_pred, passed_data)
})

