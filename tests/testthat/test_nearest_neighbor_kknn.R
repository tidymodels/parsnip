library(testthat)
library(parsnip)
library(rlang)

# ------------------------------------------------------------------------------

context("nearest neighbor execution with kknn")

num_pred <- c("Sepal.Width", "Petal.Width", "Petal.Length")
iris_bad_form <- as.formula(Species ~ term)
iris_basic <- nearest_neighbor(neighbors = 8, weight_func = "triangular") %>%
  set_engine("kknn")

ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

# ------------------------------------------------------------------------------

test_that('kknn execution', {

  skip_if_not_installed("kknn")

  # continuous
  # expect no error
  expect_error(
    fit_xy(
      iris_basic,
      control = ctrl,
      x = iris[, num_pred],
      y = iris$Sepal.Length
    ),
    regexp = NA
  )

  # nominal
  # expect no error
  expect_error(
    fit_xy(
      iris_basic,
      control = ctrl,
      x = iris[, c("Sepal.Length", "Petal.Width")],
      y = iris$Species
    ),
    regexp = NA
  )

  expect_error(
    fit(
      iris_basic,
      iris_bad_form,
      data = iris,

      control = ctrl
    )
  )

})

test_that('kknn prediction', {

  skip_if_not_installed("kknn")

  # continuous
  res_xy <- fit_xy(
    iris_basic,
    control = ctrl,
    x = iris[, num_pred],
    y = iris$Sepal.Length
  )

  uni_pred <- predict(
    res_xy$fit,
    newdata = iris[1:5, num_pred]
  )

  expect_equal(uni_pred, predict_numeric(res_xy, iris[1:5, num_pred]))

  # nominal
  res_xy_nom <- fit_xy(
    iris_basic,
    control = ctrl,
    x = iris[, c("Sepal.Length", "Petal.Width")],
    y = iris$Species
  )

  uni_pred_nom <- predict(
    res_xy_nom$fit,
    newdata = iris[1:5, c("Sepal.Length", "Petal.Width")]
  )

  expect_equal(uni_pred_nom, predict_class(res_xy_nom, iris[1:5, c("Sepal.Length", "Petal.Width")]))

  # continuous - formula interface
  res_form <- fit(
    iris_basic,
    Sepal.Length ~ log(Sepal.Width) + Species,
    data = iris,
    control = ctrl
  )

  form_pred <- predict(
    res_form$fit,
    newdata = iris[1:5,]
  )

  expect_equal(form_pred, predict_numeric(res_form, iris[1:5, c("Sepal.Width", "Species")]))
})
