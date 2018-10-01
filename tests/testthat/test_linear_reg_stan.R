library(testthat)
context("linear regression execution with stan")
library(parsnip)
library(rlang)

###################################################################

num_pred <- c("Sepal.Width", "Petal.Width", "Petal.Length")
iris_bad_form <- as.formula(Species ~ term)
iris_basic <- linear_reg(others = list(seed = 10, chains = 1))
ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

test_that('stan_glm execution', {

  skip_if_not_installed("rstanarm")

  library(rstanarm)

  # passes interactively but not on R CMD check
  expect_error(
    res <- fit(
      iris_basic,
      Sepal.Width ~ log(Sepal.Length) + Species,
      data = iris,
      control = ctrl,
      engine = "stan"
    ),
    regexp = NA
  )
  expect_error(
    res <- fit_xy(
      iris_basic,
      x = iris[, num_pred],
      y = iris$Sepal.Length,
      engine = "stan",
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    res <- fit(
      iris_basic,
      Species ~ term,
      data = iris,
      engine = "stan",
      control = ctrl
    )
  )

})

