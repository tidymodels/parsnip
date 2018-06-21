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

test_that('stan_glm execution', {
  skip_on_cran()
  iris_basic_stan <- linear_reg(others = list(seed = 1333))
  
  expect_error(
    res <- fit(
      iris_basic,
      iris_bad_form,
      data = iris,
      engine = "stan",
      control = ctrl
    )
  )
  
  stan_xy_catch <- fit(
    iris_basic,
    engine = "stan",
    control = caught_ctrl,
    x = iris[, num_pred],
    y = factor(iris$Sepal.Length)
  )
  expect_true(inherits(stan_xy_catch$fit, "try-error"))
})

