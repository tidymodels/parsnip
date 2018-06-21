library(testthat)
library(parsnip)
library(rlang)

data("lending_club")
lending_club <- head(lending_club, 200)
lc_form <- as.formula(Class ~ log(funded_amnt) + int_rate)
num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")
lc_bad_form <- as.formula(funded_amnt ~ term)
lc_basic <- logistic_reg()
ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

test_that('stan_glm execution', {
  skip_on_cran()
  lc_basic_stan <- logistic_reg(others = list(seed = 1333))
  
  expect_error(
    res <- fit(
      lc_basic,
      lc_bad_form,
      data = lending_club,
      engine = "stan",
      control = ctrl
    )
  )
  
  stan_xy_catch <- fit(
    lc_basic,
    engine = "stan",
    control = caught_ctrl,
    x = lending_club[, num_pred],
    y = lending_club$total_bal_il
  )
  expect_true(inherits(stan_xy_catch$fit, "try-error"))
  
})

