library(testthat)
library(parsnip)
library(rlang)

###################################################################

iris_form <- as.formula(Sepal.Width ~ log(Sepal.Length) + Species)
num_pred <- c("Sepal.Width", "Petal.Width", "Petal.Width")
iris_bad_form <- as.formula(Species ~ term)
iris_basic <- linear_reg()
ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

test_that('glmnet execution', {
  skip_on_cran()
  
  expect_error(
    fit(
      iris_basic,
      engine = "glmnet",
      control = ctrl,
      x = iris[, num_pred],
      y = iris$Sepal.Length
    ),
    regexp = NA
  )
  
  expect_error(
    fit(
      iris_basic,
      iris_bad_form,
      data = iris,
      engine = "glm",
      control = ctrl
    )
  )
  
  glmnet_xy_catch <- fit(
    iris_basic,
    x = iris[, num_pred],
    y = factor(iris$Sepal.Length),
    engine = "glmnet",
    control = caught_ctrl
  )
  expect_true(inherits(glmnet_xy_catch$fit, "try-error"))
  
})
