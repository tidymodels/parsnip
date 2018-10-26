library(testthat)
library(parsnip)
library(tibble)
library(rlang)

# ------------------------------------------------------------------------------

context("random forest execution with ranger")

# ------------------------------------------------------------------------------

data("lending_club")
lending_club <- head(lending_club, 200)
num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")

lc_basic <- rand_forest() %>% set_engine("ranger")
lc_ranger <- rand_forest() %>% set_engine("ranger", seed = 144)

bad_ranger_cls <- rand_forest() %>% set_engine("ranger", replace = "bad")
bad_rf_cls <- rand_forest() %>% set_engine("ranger", sampsize = -10)

ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

# ------------------------------------------------------------------------------

test_that('ranger classification execution', {

  skip_if_not_installed("ranger")

  expect_error(
    res <- fit(
      lc_ranger,
      Class ~ funded_amnt + term,
      data = lending_club,

      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    res <- fit_xy(
      lc_ranger,
      x = lending_club[, num_pred],
      y = lending_club$Class,

      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    res <- fit(
      bad_ranger_cls,
      funded_amnt ~ term,
      data = lending_club,

      control = ctrl
    )
  )

  ranger_form_catch <- fit(
    bad_ranger_cls,
    funded_amnt ~ term,
    data = lending_club,

    control = caught_ctrl
  )
  expect_true(inherits(ranger_form_catch$fit, "try-error"))

  ranger_xy_catch <- fit_xy(
    bad_ranger_cls,

    control = caught_ctrl,
    x = lending_club[, num_pred],
    y = lending_club$total_bal_il
  )
  expect_true(inherits(ranger_xy_catch$fit, "try-error"))

})

test_that('ranger classification prediction', {

  skip_if_not_installed("ranger")

  xy_fit <- fit_xy(
    rand_forest() %>% set_engine("ranger"),
    x = lending_club[, num_pred],
    y = lending_club$Class,

    control = ctrl
  )

  xy_pred <- predict(xy_fit$fit, data = lending_club[1:6, num_pred])$prediction
  xy_pred <- colnames(xy_pred)[apply(xy_pred, 1, which.max)]
  xy_pred <- factor(xy_pred, levels = levels(lending_club$Class))
  expect_equal(xy_pred, predict_class(xy_fit, new_data = lending_club[1:6, num_pred]))

  form_fit <- fit(
    rand_forest() %>% set_engine("ranger"),
    Class ~ funded_amnt + int_rate,
    data = lending_club,

    control = ctrl
  )

  form_pred <- predict(form_fit$fit, data = lending_club[1:6, c("funded_amnt", "int_rate")])$prediction
  form_pred <- colnames(form_pred)[apply(form_pred, 1, which.max)]
  form_pred <- factor(form_pred, levels = levels(lending_club$Class))
  expect_equal(form_pred, predict_class(form_fit, new_data = lending_club[1:6, c("funded_amnt", "int_rate")]))

})


test_that('ranger classification probabilities', {

  skip_if_not_installed("ranger")

  xy_fit <- fit_xy(
    rand_forest() %>% set_engine("ranger", seed = 3566),
    x = lending_club[, num_pred],
    y = lending_club$Class,

    control = ctrl
  )

  xy_pred <- predict(xy_fit$fit, data = lending_club[1:6, num_pred])$predictions
  xy_pred <- as_tibble(xy_pred)
  expect_equal(xy_pred, predict_classprob(xy_fit, new_data = lending_club[1:6, num_pred]))

  one_row <- predict_classprob(xy_fit, new_data = lending_club[1, num_pred])
  expect_equivalent(xy_pred[1,], one_row)

  form_fit <- fit(
    rand_forest()  %>% set_engine("ranger", seed = 3566),
    Class ~ funded_amnt + int_rate,
    data = lending_club,

    control = ctrl
  )

  form_pred <- predict(form_fit$fit, data = lending_club[1:6, c("funded_amnt", "int_rate")])$predictions
  form_pred <- as_tibble(form_pred)
  expect_equal(form_pred, predict_classprob(form_fit, new_data = lending_club[1:6, c("funded_amnt", "int_rate")]))

  no_prob_model <- fit_xy(
    rand_forest() %>% set_engine("ranger", probability = FALSE),
    x = lending_club[, num_pred],
    y = lending_club$Class,

    control = ctrl
  )

  expect_error(
    predict_classprob(no_prob_model, new_data = lending_club[1:6, num_pred])
  )
})

# ------------------------------------------------------------------------------

num_pred <- names(mtcars)[3:6]

car_basic <- rand_forest() %>% set_engine("ranger")

bad_ranger_reg <- rand_forest() %>% set_engine("ranger", replace = "bad")
bad_rf_reg <- rand_forest() %>% set_engine("ranger", sampsize = -10)

ctrl <- list(verbosity = 1, catch = FALSE)
caught_ctrl <- list(verbosity = 1, catch = TRUE)
quiet_ctrl <- list(verbosity = 0, catch = TRUE)

# ------------------------------------------------------------------------------

test_that('ranger regression execution', {

  skip_if_not_installed("ranger")

  expect_error(
    res <- fit(
      car_basic,
      mpg ~ .,
      data = mtcars,
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    res <- fit_xy(
      car_basic,
      x = mtcars,
      y = mtcars$mpg,
      control = ctrl
    ),
    regexp = NA
  )


  ranger_form_catch <- fit(
    bad_ranger_reg,
    mpg ~ .,
    data = mtcars,
    control = caught_ctrl
  )
  expect_true(inherits(ranger_form_catch$fit, "try-error"))

  ranger_xy_catch <- fit_xy(
    bad_ranger_reg,
    control = caught_ctrl,
    x = mtcars[, num_pred],
    y = mtcars$mpg
  )
  expect_true(inherits(ranger_xy_catch$fit, "try-error"))

})

test_that('ranger regression prediction', {

  skip_if_not_installed("ranger")

  xy_fit <- fit_xy(
    car_basic,
    x = mtcars[, -1],
    y = mtcars$mpg,
    control = ctrl
  )

  xy_pred <- predict(xy_fit$fit, data = tail(mtcars[, -1]))$prediction

  expect_equal(xy_pred, predict_numeric(xy_fit, new_data = tail(mtcars[, -1])))

})


test_that('ranger regression intervals', {

  skip_if_not_installed("ranger")

  xy_fit <- fit_xy(
    rand_forest() %>% set_engine("ranger", keep.inbag = TRUE),
    x = mtcars[, -1],
    y = mtcars$mpg,
    control = ctrl
  )

  rgr_pred <- predict(xy_fit$fit, data = tail(mtcars[, -1]))$predictions
  rgr_se <-
    expect_warning(predict(xy_fit$fit, data = tail(mtcars[, -1]), type = "se")$se)
  rgr_lower <- rgr_pred - qnorm(0.035, lower.tail = FALSE) * rgr_se
  rgr_upper <- rgr_pred + qnorm(0.035, lower.tail = FALSE) * rgr_se

  parsnip_int <-
    expect_warning(
      predict(xy_fit, new_data = tail(mtcars[, -1]),
              type = "conf_int", std_error = TRUE, level = 0.93
      )
    )
  expect_equal(rgr_lower, parsnip_int$.pred_lower)
  expect_equal(rgr_upper, parsnip_int$.pred_upper)
  expect_equal(rgr_se, parsnip_int$.std_error)

})



test_that('additional descriptor tests', {

  skip_if_not_installed("ranger")

  descr_xy <- fit_xy(
    rand_forest(mtry = floor(sqrt(.cols())) + 1) %>% set_engine("ranger"),
    x = mtcars[, -1],
    y = mtcars$mpg,
    control = ctrl
  )
  expect_equal(descr_xy$fit$mtry, 4)

  descr_f <- fit(
    rand_forest(mtry = floor(sqrt(.cols())) + 1) %>% set_engine("ranger"),
    mpg ~ ., data = mtcars,
    control = ctrl
  )
  expect_equal(descr_f$fit$mtry, 4)

  descr_xy <- fit_xy(
    rand_forest(mtry = floor(sqrt(.cols())) + 1) %>% set_engine("ranger"),
    x = mtcars[, -1],
    y = mtcars$mpg,
    control = ctrl
  )
  expect_equal(descr_xy$fit$mtry, 4)

  descr_f <- fit(
    rand_forest(mtry = floor(sqrt(.cols())) + 1) %>% set_engine("ranger"),
    mpg ~ ., data = mtcars,
    control = ctrl
  )
  expect_equal(descr_f$fit$mtry, 4)

  ##

  exp_wts <- quo(c(min(.lvls()), 20, 10))

  descr_other_xy <- fit_xy(
    rand_forest(mtry = 2) %>%
      set_engine("ranger", class.weights = c(min(.lvls()), 20, 10)),
    x = iris[, 1:4],
    y = iris$Species,
    control = ctrl
  )
  expect_equal(descr_other_xy$fit$mtry, 2)
  expect_equal(descr_other_xy$fit$call$class.weights, exp_wts)

  descr_other_f <- fit(
    rand_forest(mtry = 2) %>%
      set_engine("ranger", class.weights = c(min(.lvls()), 20, 10)),
    Species ~ ., data = iris,
    control = ctrl
  )
  expect_equal(descr_other_f$fit$mtry, 2)
  expect_equal(descr_other_f$fit$call$class.weights, exp_wts)

  descr_other_xy <- fit_xy(
    rand_forest(mtry = 2) %>%
      set_engine("ranger", class.weights = c(min(.lvls()), 20, 10)),
    x = iris[, 1:4],
    y = iris$Species,
    control = ctrl
  )
  expect_equal(descr_other_xy$fit$mtry, 2)
  expect_equal(descr_other_xy$fit$call$class.weights, exp_wts)

  descr_other_f <- fit(
    rand_forest(mtry = 2) %>%
      set_engine("ranger", class.weights = c(min(.lvls()), 20, 10)),
    Species ~ ., data = iris,
    control = ctrl
  )
  expect_equal(descr_other_f$fit$mtry, 2)
  expect_equal(descr_other_f$fit$call$class.weights, exp_wts)
})


test_that('ranger classification prediction', {

  skip_if_not_installed("ranger")

  xy_class_fit <-
    rand_forest()  %>% set_engine("ranger") %>%
    fit_xy(
      x = iris[, 1:4],
      y = iris$Species,
      control = ctrl
    )

  xy_class_pred <- predict(xy_class_fit$fit, data = iris[c(1, 51, 101), 1:4])$prediction
  xy_class_pred <- colnames(xy_class_pred)[apply(xy_class_pred, 1, which.max)]
  xy_class_pred <- factor(xy_class_pred, levels = levels(iris$Species))

  expect_equal(
    xy_class_pred,
    predict(xy_class_fit, new_data = iris[c(1, 51, 101), 1:4])$.pred_class
  )

  xy_prob_fit <-
    rand_forest() %>%
    set_engine("ranger") %>%
    fit_xy(
      x = iris[, 1:4],
      y = iris$Species,
      control = ctrl
    )

  xy_prob_pred <- predict(xy_prob_fit$fit, data = iris[c(1, 51, 101), 1:4])$prediction
  xy_prob_pred <- colnames(xy_prob_pred)[apply(xy_prob_pred, 1, which.max)]
  xy_prob_pred <- factor(xy_prob_pred, levels = levels(iris$Species))

  expect_equal(
    xy_class_pred,
    predict(xy_prob_fit, new_data = iris[c(1, 51, 101), 1:4])$.pred_class
  )

  xy_prob_prob <- predict(xy_prob_fit$fit, data = iris[c(1, 51, 101), 1:4], type = "response")
  xy_prob_prob <- as_tibble(xy_prob_prob$prediction)
  names(xy_prob_prob) <- paste0(".pred_", names(xy_prob_prob))
  expect_equal(
    xy_prob_prob,
    predict(xy_prob_fit, new_data = iris[c(1, 51, 101), 1:4], type = "prob")
  )
})


test_that('ranger classification intervals', {

  skip_if_not_installed("ranger")

  lc_fit <- fit(
    rand_forest() %>%
      set_engine("ranger", keep.inbag = TRUE, probability = TRUE),
    Class ~ funded_amnt + int_rate,
    data = lending_club,
    control = ctrl
  )

  rgr_pred <- predict(lc_fit$fit, data = tail(lending_club))$predictions
  rgr_se <- expect_warning(predict(lc_fit$fit, data = tail(lending_club), type = "se")$se)
  rgr_lower <- rgr_pred - qnorm(0.035, lower.tail = FALSE) * rgr_se
  rgr_upper <- rgr_pred + qnorm(0.035, lower.tail = FALSE) * rgr_se
  rgr_lower[rgr_lower < 0] <- 0
  rgr_upper[rgr_upper > 1] <- 1

  parsnip_int <-
    expect_warning(
      predict(lc_fit, new_data = tail(lending_club),
              type = "conf_int", std_error = TRUE, level = 0.93
      )
    )
  expect_equal(rgr_lower[, "bad"], parsnip_int$.pred_lower_bad)
  expect_equal(rgr_lower[, "good"], parsnip_int$.pred_lower_good)
  expect_equal(rgr_upper[, "bad"], parsnip_int$.pred_upper_bad)
  expect_equal(rgr_upper[, "good"], parsnip_int$.pred_upper_good)
  expect_equal(rgr_se[, 1], parsnip_int$.std_error_bad)
  expect_equal(rgr_se[, 2], parsnip_int$.std_error_good)

})

