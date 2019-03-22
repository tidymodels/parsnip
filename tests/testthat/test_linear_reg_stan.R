library(testthat)
library(parsnip)
library(rlang)

# ------------------------------------------------------------------------------

context("linear regression execution with stan")

num_pred <- c("Sepal.Width", "Petal.Width", "Petal.Length")
iris_bad_form <- as.formula(Species ~ term)
iris_basic <- linear_reg() %>%
  set_engine("stan", seed = 10, chains = 1)

ctrl <- fit_control(verbosity = 0L, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 0L, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0L, catch = TRUE)

# ------------------------------------------------------------------------------

test_that('stan_glm execution', {
  skip_if_not_installed("rstanarm")

  expect_error(
    res <- fit(
      iris_basic,
      Sepal.Width ~ log(Sepal.Length) + Species,
      data = iris,
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- fit_xy(
      iris_basic,
      x = iris[, num_pred],
      y = iris$Sepal.Length,
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    res <- fit(
      iris_basic,
      Species ~ term,
      data = iris,
      control = ctrl
    )
  )

})


test_that('stan prediction', {
  skip_if_not_installed("rstanarm")

  uni_pred <- c(5.01531691055198, 4.6896592504705, 4.74907435900005, 4.82563873798984,
                5.08044844256827)
  inl_pred <- c(3.47062722437493, 3.38380776677489, 3.29336980560884, 3.24669710332179,
                3.42765162180813)

  res_xy <- fit_xy(
    linear_reg() %>%
      set_engine("stan", seed = 10, chains = 1),
    x = iris[, num_pred],
    y = iris$Sepal.Length,
    control = quiet_ctrl
  )

  expect_equal(uni_pred, predict(res_xy, iris[1:5, num_pred])$.pred, tolerance = 0.001)

  res_form <- fit(
    iris_basic,
    Sepal.Width ~ log(Sepal.Length) + Species,
    data = iris,
    control = quiet_ctrl
  )
  expect_equal(inl_pred, predict(res_form, iris[1:5, ])$.pred, tolerance = 0.001)
})


test_that('stan intervals', {
  skip_if_not_installed("rstanarm")

  res_xy <- fit_xy(
    linear_reg() %>%
      set_engine("stan", seed = 1333, chains = 10, iter = 1000),
    x = iris[, num_pred],
    y = iris$Sepal.Length,
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

  ci_lower <- c(4.93164991101342, 4.60197941230393, 4.6671442757811, 4.74402724639963,
               4.99248110476701)
  ci_upper <- c(5.1002837047058, 4.77617561853506, 4.83183673602725, 4.90844811805409,
                5.16979395659009)

  pi_lower <- c(4.43202758985944, 4.09957733046886, 4.17664779714598, 4.24948546338885,
                4.50058914781073)
  pi_upper <- c(5.59783267637042, 5.25976504318669, 5.33296516452929, 5.41050668003565,
                5.66355828140989)

  expect_equivalent(confidence_parsnip$.pred_lower, ci_lower)
  expect_equivalent(confidence_parsnip$.pred_upper, ci_upper)

  expect_equivalent(prediction_parsnip$.pred_lower,
                    pi_lower,
                    tol = 0.01)
  expect_equivalent(prediction_parsnip$.pred_upper,
                    pi_upper,
                    tol = 0.01)
})



