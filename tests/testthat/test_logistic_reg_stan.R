library(testthat)
library(parsnip)
library(rlang)
library(tibble)

# ------------------------------------------------------------------------------

context("execution tests for stan logistic regression")

data("lending_club")
lending_club <- head(lending_club, 200)
lc_form <- as.formula(Class ~ log(funded_amnt) + int_rate)
num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")
lc_basic <-
  logistic_reg() %>%
  set_engine("stan", seed = 1333, chains = 1)

ctrl <- fit_control(verbosity = 0, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 0, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

# ------------------------------------------------------------------------------

test_that('stan_glm execution', {
  skip_if_not_installed("rstanarm")

  expect_error(
    res <- fit(
      lc_basic,
      funded_amnt ~ term,
      data = lending_club,
      control = ctrl
    )
  )

  expect_error(
    fit_xy(
      lc_basic,
      control = caught_ctrl,
      x = lending_club[, num_pred],
      y = lending_club$total_bal_il
    )
  )

})


test_that('stan_glm prediction', {
  skip_if_not_installed("rstanarm")

  xy_fit <- fit_xy(
    logistic_reg() %>%
      set_engine("stan", seed = 1333, chains = 1),
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  xy_pred <- structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c("bad", "good"), class = "factor")

  expect_equal(xy_pred, parsnip:::predict_class.model_fit(xy_fit, lending_club[1:7, num_pred]))

  res_form <- fit(
    logistic_reg() %>%
      set_engine("stan", seed = 1333, chains = 1),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    control = ctrl
  )

  form_pred <- structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L),
                         .Label = c("bad", "good"),
                         class = "factor")

  expect_equal(form_pred, parsnip:::predict_class.model_fit(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]))

})



test_that('stan_glm probability', {
  skip_if_not_installed("rstanarm")

  xy_fit <- fit_xy(
    logistic_reg() %>%
      set_engine("stan", seed = 1333, chains = 1),
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  xy_pred <-
    tibble::tribble(
    ~bad,             ~good,
    0.0173511241321764, 0.982648875867824,
    0.0550090130462705,  0.94499098695373,
    0.0292445716644468, 0.970755428335553,
    0.0516116810109397,  0.94838831898906,
    0.0142530690940691, 0.985746930905931,
    0.0184806465081366, 0.981519353491863,
    0.0253642111906806, 0.974635788809319
  )

  expect_equal(
    xy_pred %>% as.data.frame(),
    parsnip:::predict_classprob.model_fit(xy_fit, lending_club[1:7, num_pred]) %>% as.data.frame()
  )

  res_form <- fit(
    logistic_reg() %>%
      set_engine("stan", seed = 1333, chains = 1),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    control = ctrl
  )

  form_pred <-
    tibble::tribble(
      ~bad,             ~good,
      0.0451516541621074, 0.954848345837893,
      0.0663232780491584, 0.933676721950842,
      0.0425128897715562, 0.957487110228444,
      0.0442197030195933, 0.955780296980407,
      0.00135166763321781, 0.998648332366782,
      0.013776487556396, 0.986223512443604,
      0.00359938202445076, 0.996400617975549
    )
  expect_equal(
    form_pred %>% as.data.frame(),
    parsnip:::predict_classprob.model_fit(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]) %>%
      as.data.frame()
  )
})


test_that('stan intervals', {
  skip_if_not_installed("rstanarm")

  res_form <- fit(
    logistic_reg() %>%
      set_engine("stan", seed = 1333, chains = 1),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    control = ctrl
  )

  set.seed(555)
  confidence_parsnip <-
    predict(res_form,
            new_data = lending_club[1:5,],
            type = "conf_int",
            level = 0.93,
            std_error = TRUE)

  set.seed(555)
  prediction_parsnip <-
    predict(res_form,
            new_data = lending_club[1:5,],
            type = "pred_int",
            level = 0.93,
            std_error = TRUE)

  stan_lower <-
    c(`1` = 0.913925483690233, `2` = 0.841801274737206, `3` = 0.91056642931229,
      `4` = 0.913619668586545, `5` = 0.987780279394871)
  stan_upper <-
    c(`1` = 0.978674663115785, `2` = 0.975178762720162, `3` = 0.984417491942267,
      `4` = 0.979606072215269, `5` = 0.9999049778978)
  stan_std <-
    c(`1` = 0.0181025303127182, `2` = 0.0388665155739319, `3` = 0.0205886091162274,
      `4` = 0.0181715224502082, `5` = 0.00405145389896896)

  expect_equivalent(confidence_parsnip$.pred_lower_good, stan_lower)
  expect_equivalent(confidence_parsnip$.pred_upper_good, stan_upper)
  expect_equivalent(confidence_parsnip$.pred_lower_bad, 1 - stan_upper)
  expect_equivalent(confidence_parsnip$.pred_upper_bad, 1 - stan_lower)
  expect_equivalent(confidence_parsnip$.std_error, stan_std)

  stan_pred_lower <- c(`1` = 0, `2` = 0, `3` = 0, `4` = 0, `5` = 1)
  stan_pred_upper <- c(`1` = 1, `2` = 1, `3` = 1, `4` = 1, `5` = 1)
  stan_pred_std  <-
    c(`1` = 0.211744742168102, `2` = 0.265130711714607, `3` = 0.209589904165081,
      `4` = 0.198389410902796, `5` = 0.0446989708829856)
  expect_equivalent(prediction_parsnip$.pred_lower_good, stan_pred_lower)
  expect_equivalent(prediction_parsnip$.pred_upper_good, stan_pred_upper)
  expect_equivalent(prediction_parsnip$.std_error, stan_pred_std, tolerance = 0.1)
})



