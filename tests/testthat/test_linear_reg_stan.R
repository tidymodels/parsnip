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


test_that('stan prediction', {

  skip_if_not_installed("rstanarm")
  library(rstanarm)

  uni_stan <- stan_glm(Sepal.Length ~ Sepal.Width + Petal.Width + Petal.Length, data = iris, seed = 123)
  uni_pred <- unname(predict(uni_stan, newdata = iris[1:5, ]))
  inl_stan <- stan_glm(Sepal.Width ~ log(Sepal.Length) + Species, data = iris, seed = 123, chains = 1)
  inl_pred <- unname(predict(inl_stan, newdata = iris[1:5, c("Sepal.Length", "Species")]))

  res_xy <- fit_xy(
    linear_reg(others = list(seed = 123, chains = 1)),
    x = iris[, num_pred],
    y = iris$Sepal.Length,
    engine = "stan",
    control = ctrl
  )

  expect_equal(uni_pred, predict_num(res_xy, iris[1:5, num_pred]), tolerance = 0.001)

  res_form <- fit(
    iris_basic,
    Sepal.Width ~ log(Sepal.Length) + Species,
    data = iris,
    engine = "stan",
    control = ctrl
  )
  expect_equal(inl_pred, predict_num(res_form, iris[1:5, ]), tolerance = 0.001)
})


test_that('stan intervals', {
  skip_if_not_installed("rstanarm")
  library(rstanarm)

  res_xy <- fit_xy(
    linear_reg(others = list(seed = 1333, chains = 10, iter = 1000)),
    x = iris[, num_pred],
    y = iris$Sepal.Length,
    engine = "stan",
    control = quiet_ctrl
  )

  confidence_parsnip <-
    predict(res_xy,
            new_data = iris[1:5,],
            type = "conf_int",
            level = 0.93)

  prediction_parsnip <-
    predict(res_xy,
            new_data = iris[1:5,],
            type = "pred_int",
            level = 0.93)

  prediction_stan <-
    predictive_interval(res_xy$fit, newdata = iris[1:5, ], seed = 13,
                        prob = 0.93)

  stan_post <- posterior_linpred(res_xy$fit, newdata = iris[1:5, ],
                                 seed = 13)
  stan_lower <- apply(stan_post, 2, quantile, prob = 0.035)
  stan_upper <- apply(stan_post, 2, quantile, prob = 0.965)

  expect_equivalent(confidence_parsnip$.pred_lower, stan_lower)
  expect_equivalent(confidence_parsnip$.pred_upper, stan_upper)

  expect_equivalent(prediction_parsnip$.pred_lower,
                    prediction_stan[, "3.5%"],
                    tol = 0.01)
  expect_equivalent(prediction_parsnip$.pred_upper,
                    prediction_stan[, "96.5%"],
                    tol = 0.01)
})



