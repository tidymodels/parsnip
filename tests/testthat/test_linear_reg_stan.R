library(testthat)
library(parsnip)
library(rlang)
library(rstanarm)

###################################################################

iris_form <- as.formula(Sepal.Width ~ log(Sepal.Length) + Species)
num_pred <- c("Sepal.Width", "Petal.Width", "Petal.Length")
iris_bad_form <- as.formula(Species ~ term)
iris_basic <- linear_reg()
ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

test_that('stan_glm execution', {
  skip_on_cran()
  iris_basic_stan <- linear_reg(others = list(seed = 1333))

  # passes interactively but not on R CMD check
  expect_error(
    res <- fit(
      iris_basic,
      iris_form,
      data = iris,
      control = ctrl,
      engine = "stan"
    ),
    regexp = NA
  )
  expect_error(
    res <- fit(
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
      iris_bad_form,
      data = iris,
      engine = "stan",
      control = ctrl
    )
  )

})


test_that('stan prediction', {
  uni_stan <- stan_glm(Sepal.Length ~ Sepal.Width + Petal.Width + Petal.Length, data = iris, seed = 123)
  uni_pred <- unname(predict(uni_stan, newdata = iris[1:5, ]))
  inl_stan <- stan_glm(iris_form, data = iris, seed = 123)
  inl_pred <- unname(predict(inl_stan, newdata = iris[1:5, c("Sepal.Length", "Species")]))

  res_xy <- fit(
    linear_reg(others = list(seed = 123)),
    x = iris[, num_pred],
    y = iris$Sepal.Length,
    engine = "stan",
    control = ctrl
  )

  expect_equal(uni_pred, predict(res_xy, iris[1:5, num_pred]), tolerance = 0.001)

  res_form <- fit(
    iris_basic,
    iris_form,
    data = iris,
    engine = "stan",
    control = ctrl
  )
  expect_equal(inl_pred, predict(res_form, iris[1:5, ]), tolerance = 0.001)
})
