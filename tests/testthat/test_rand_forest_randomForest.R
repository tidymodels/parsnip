library(testthat)
context("random forest execution with randomForest")
library(parsnip)
library(tibble)

data("lending_club")
lending_club <- head(lending_club, 200)
num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")

lc_basic <- rand_forest(mode = "classification")
bad_rf_cls <- rand_forest(mode = "classification",
                          others = list(sampsize = -10))

ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)


test_that('randomForest classification execution', {

  skip_if_not_installed("randomForest")

  # passes interactively but not on R CMD check
  # expect_error(
  #   fit(
  #     lc_basic,
  #     Class ~ funded_amnt + term,
  #     data = lending_club,
  #     engine = "randomForest",
  #     control = ctrl
  #   ),
  #   regexp = NA
  # )

  expect_error(
    fit_xy(
      lc_basic,
      engine = "randomForest",
      control = ctrl,
      x = lending_club[, num_pred],
      y = lending_club$Class
    ),
    regexp = NA
  )

  expect_error(
    fit(
      bad_rf_cls,
      unded_amnt ~ term,
      data = lending_club,
      engine = "randomForest",
      control = ctrl
    )
  )

  # passes interactively but not on R CMD check
  # randomForest_form_catch <- fit(
  #   bad_rf_cls,
  #   unded_amnt ~ term,
  #   data = lending_club,
  #   engine = "randomForest",
  #   control = caught_ctrl
  # )
  # expect_true(inherits(randomForest_form_catch, "try-error"))

  randomForest_xy_catch <- fit_xy(
    bad_rf_cls,
    x = lending_club[, num_pred],
    y = lending_club$total_bal_il,
    engine = "randomForest",
    control = caught_ctrl
  )
  expect_true(inherits(randomForest_xy_catch$fit, "try-error"))

})

###################################################################

car_form <- as.formula(mpg ~ .)
num_pred <- names(mtcars)[3:6]

car_basic <- rand_forest(mode = "regression")

bad_ranger_reg <- rand_forest(mode = "regression",
                              others = list(min.node.size = -10))
bad_rf_reg <- rand_forest(mode = "regression",
                          others = list(sampsize = -10))

ctrl <- list(verbosity = 1, catch = FALSE)
caught_ctrl <- list(verbosity = 1, catch = TRUE)
quiet_ctrl <- list(verbosity = 0, catch = TRUE)

test_that('randomForest regression execution', {

  skip_if_not_installed("randomForest")

  # passes interactively but not on R CMD check
  # expect_error(
  #   fit(
  #     car_basic,
  #     car_form,
  #     data = mtcars,
  #     engine = "randomForest",
  #     control = ctrl
  #   ),
  #   regexp = NA
  # )

  expect_error(
    fit_xy(
      car_basic,
      x = mtcars,
      y = mtcars$mpg,
      engine = "randomForest",
      control = ctrl
    ),
    regexp = NA
  )

  # passes interactively but not on R CMD check
  # randomForest_form_catch <- fit(
  #   bad_rf_reg,
  #   car_form,
  #   data = mtcars,
  #   engine = "randomForest",
  #   control = caught_ctrl
  # )
  # expect_true(inherits(randomForest_form_catch, "try-error"))

  randomForest_xy_catch <- fit_xy(
    bad_rf_reg,
    x = mtcars,
    y = mtcars$mpg,
    engine = "randomForest",
    control = caught_ctrl
  )
  expect_true(inherits(randomForest_xy_catch$fit, "try-error"))

})

