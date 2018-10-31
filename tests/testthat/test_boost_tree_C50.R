library(testthat)
library(parsnip)
library(tibble)

# ------------------------------------------------------------------------------

context("boosted tree execution with C5.0")

data("lending_club")
lending_club <- head(lending_club, 200)
num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")
lc_basic <-
  boost_tree(mode = "classification")  %>%
      set_engine("C5.0")

ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

# ------------------------------------------------------------------------------

test_that('C5.0 execution', {

  skip_if_not_installed("C50")

  expect_error(
    res <- fit(
      lc_basic,
      Class ~ log(funded_amnt) + int_rate,
      data = lending_club,
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- fit_xy(
      lc_basic,
      x = lending_club[, num_pred],
      y = lending_club$Class,
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

  C5.0_form_catch <- fit(
    lc_basic,
    funded_amnt ~ term,
    data = lending_club,
    control = caught_ctrl
  )
  expect_true(inherits(C5.0_form_catch$fit, "try-error"))

  C5.0_xy_catch <- fit_xy(
    lc_basic,
    control = caught_ctrl,
    x = lending_club[, num_pred],
    y = lending_club$total_bal_il
  )
  expect_true(inherits(C5.0_xy_catch$fit, "try-error"))
})

test_that('C5.0 prediction', {

  skip_if_not_installed("C50")

  classes_xy <- fit_xy(
    lc_basic,
    x = lending_club[, num_pred],
    y = lending_club$Class,
    control = ctrl
  )

  xy_pred <- predict(classes_xy$fit, newdata = lending_club[1:7, num_pred])
  expect_equal(xy_pred, predict_class(classes_xy, lending_club[1:7, num_pred]))

})

test_that('C5.0 probabilities', {

  skip_if_not_installed("C50")

  classes_xy <- fit_xy(
    lc_basic,
    x = lending_club[, num_pred],
    y = lending_club$Class,
    control = ctrl
  )

  xy_pred <- predict(classes_xy$fit, newdata = as.data.frame(lending_club[1:7, num_pred]), type = "prob")
  xy_pred <- as_tibble(xy_pred)
  expect_equal(xy_pred, predict_classprob(classes_xy, lending_club[1:7, num_pred]))

  one_row <- predict_classprob(classes_xy, lending_club[1, num_pred])
  expect_equal(xy_pred[1,], one_row)

})


test_that('submodel prediction', {

  skip_if_not_installed("C50")

  vars <- c("female", "tenure", "total_charges", "phone_service", "monthly_charges")
  class_fit <-
    boost_tree(trees = 20, mode = "classification") %>%
    set_engine("C5.0", control = C5.0Control(earlyStopping = FALSE)) %>%
    fit(churn ~ ., data = wa_churn[-(1:4), c("churn", vars)])

  pred_class <- predict(class_fit$fit, wa_churn[1:4, vars], trials = 4, type = "prob")

  mp_res <- multi_predict(class_fit, new_data = wa_churn[1:4, vars], trees = 4, type = "prob")
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred_No"]], unname(pred_class[, "No"]))

  expect_error(
    multi_predict(class_fit, newdata = wa_churn[1:4, vars], trees = 4, type = "prob"),
    "Did you mean"
  )
})

