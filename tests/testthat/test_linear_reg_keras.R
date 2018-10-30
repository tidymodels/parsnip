library(testthat)
library(parsnip)
library(rlang)
library(tibble)

# ------------------------------------------------------------------------------

context("keras linear regression")
source("helpers.R")

# ------------------------------------------------------------------------------

basic_mod <-
  linear_reg() %>%
  set_engine("keras", epochs = 50, verbose = 0)

ridge_mod <-
  linear_reg(penalty = 0.1) %>%
  set_engine("keras", epochs = 50, verbose = 0)

ctrl <- fit_control(verbosity = 0, catch = FALSE)

# ------------------------------------------------------------------------------

test_that('model fitting', {
  skip_on_cran()
  skip_if_not_installed("keras")

  set.seed(257)
  expect_error(
    fit1 <-
      fit_xy(
      basic_mod,
      control = ctrl,
      x = iris[,2:4],
      y = iris$Sepal.Length
    ),
    regexp = NA
  )

  set.seed(257)
  expect_error(
    fit2 <-
      fit_xy(
        basic_mod,
        control = ctrl,
        x = iris[,2:4],
        y = iris$Sepal.Length
      ),
    regexp = NA
  )
  expect_equal(fit1, fit2)

  expect_error(
    fit(
      basic_mod,
      Sepal.Length ~ .,
      data = iris[, -5],
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    fit1 <-
      fit_xy(
        ridge_mod,
        control = ctrl,
        x = iris[,2:4],
        y = iris$Sepal.Length
      ),
    regexp = NA
  )

  expect_error(
    fit(
      ridge_mod,
      Sepal.Length ~ .,
      data = iris[, -5],
      control = ctrl
    ),
    regexp = NA
  )

})


test_that('regression prediction', {
  skip_on_cran()
  skip_if_not_installed("keras")

  library(keras)

  set.seed(257)
  lm_fit <-
    fit_xy(
      basic_mod,
      control = ctrl,
      x = iris[,2:4],
      y = iris$Sepal.Length
    )

  keras_pred <-
    predict(lm_fit$fit, as.matrix(iris[1:3,2:4])) %>%
    as_tibble() %>%
    setNames(".pred")
  parsnip_pred <- predict(lm_fit, iris[1:3,2:4])
  expect_equal(as.data.frame(keras_pred), as.data.frame(parsnip_pred))

  set.seed(257)
  rr_fit <-
    fit_xy(
      ridge_mod,
      control = ctrl,
      x = iris[,2:4],
      y = iris$Sepal.Length
    )

  keras_pred <-
    predict(rr_fit$fit, as.matrix(iris[1:3,2:4])) %>%
    as_tibble() %>%
    setNames(".pred")
  parsnip_pred <- predict(rr_fit, iris[1:3,2:4])
  expect_equal(as.data.frame(keras_pred), as.data.frame(parsnip_pred))

})
