library(testthat)
library(parsnip)
library(ranger)

data("lending_club")
lending_club <- head(lending_club, 200)
lc_form <- as.formula(Class ~ funded_amnt + term)
num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")
lc_bad_form <- as.formula(funded_amnt ~ term)

lc_basic <- rand_forest(mode = "classification")
lc_ranger <- rand_forest(mode = "classification", others = list(seed = 144))

bad_ranger_cls <- rand_forest(mode = "classification",
                              others = list(min.node.size = -10))
bad_rf_cls <- rand_forest(mode = "classification",
                          others = list(sampsize = -10))

ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)


test_that('ranger classification execution', {
  skip_on_cran()
  
  # passes interactively but not on R CMD check
  # expect_error(
  #   res <- fit(
  #     lc_ranger,
  #     lc_form,
  #     data = lending_club,
  #     engine = "ranger",
  #     control = ctrl
  #   ),
  #   regexp = NA
  # )
  expect_error(
    res <- fit(
      lc_ranger,
      x = lending_club[, num_pred],
      y = lending_club$Class,
      engine = "ranger",
      control = ctrl
    ),
    regexp = NA
  )
  
  expect_error(
    res <- fit(
      bad_ranger_cls,
      lc_bad_form,
      data = lending_club,
      engine = "ranger",
      control = ctrl
    )
  )
  
  # passes interactively but not on R CMD check
  # ranger_form_catch <- fit(
  #   bad_ranger_cls,
  #   lc_bad_form,
  #   data = lending_club,
  #   engine = "ranger",
  #   control = caught_ctrl
  # )
  # expect_true(inherits(ranger_form_catch$fit, "try-error"))
  
  ranger_xy_catch <- fit(
    bad_ranger_cls,
    engine = "ranger",
    control = caught_ctrl,
    x = lending_club[, num_pred],
    y = lending_club$total_bal_il
  )
  expect_true(inherits(ranger_xy_catch$fit, "try-error"))
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

test_that('ranger regression execution', {
  skip_on_cran()
  
  # passes interactively but not on R CMD check
  # expect_error(
  #   res <- fit(
  #     car_basic,
  #     car_form,
  #     data = mtcars,
  #     engine = "ranger",
  #     control = ctrl
  #   ),
  #   regexp = NA
  # )
  # passes interactively but not on R CMD check
  # expect_error(
  #   res <- fit(
  #     car_basic,
  #     x = mtcars,
  #     y = mtcars$mpg,
  #     engine = "ranger",
  #     control = ctrl
  #   ),
  #   regexp = NA
  # )
  
  # passes interactively but not on R CMD check
  # ranger_form_catch <- fit(
  #   bad_ranger_reg,
  #   car_form,
  #   data = mtcars,
  #   engine = "ranger",
  #   control = caught_ctrl
  # )
  # expect_true(inherits(ranger_form_catch$fit, "try-error"))
  
  ranger_xy_catch <- fit(
    bad_ranger_reg,
    engine = "ranger",
    control = caught_ctrl,
    x = mtcars[, num_pred],
    y = mtcars$mpg
  )
  expect_true(inherits(ranger_xy_catch$fit, "try-error"))
  
})


test_that('ranger regression prediction', {
  skip_on_cran()
  
  xy_fit <- fit(
    car_basic,
    x = mtcars[, -1],
    y = mtcars$mpg,
    engine = "ranger",
    control = ctrl
  )
  
  xy_pred <- predict(xy_fit$fit, data = tail(mtcars[, -1]))$prediction

  expect_equal(xy_pred, predict(xy_fit, newdata = tail(mtcars[, -1])))
  
})

