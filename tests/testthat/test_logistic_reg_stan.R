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

  xy_pred <-
    predict(xy_fit$fit,
            newdata = lending_club[1:7, num_pred])
  xy_pred <- xy_fit$fit$family$linkinv(xy_pred)
  xy_pred <- ifelse(xy_pred >= 0.5, "good", "bad")
  xy_pred <- factor(xy_pred, levels = levels(lending_club$Class))
  xy_pred <- unname(xy_pred)

  expect_equal(xy_pred, predict_class(xy_fit, lending_club[1:7, num_pred]))

  res_form <- fit(
    logistic_reg() %>%
      set_engine("stan", seed = 1333, chains = 1),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    control = ctrl
  )

  # form_pred <-
  #   predict(res_form$fit,
  #           newdata = lending_club[1:7, c("funded_amnt", "int_rate")])
  # form_pred <- xy_fit$fit$family$linkinv(form_pred)
  # form_pred <- unname(form_pred)
  # form_pred <- ifelse(form_pred >= 0.5, "good", "bad")
  # form_pred <- factor(form_pred, levels = levels(lending_club$Class))
  form_pred <- structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L),
                         .Label = c("bad", "good"),
                         class = "factor")
  expect_equal(form_pred, predict_class(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]))
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
    predict(xy_fit$fit,
            newdata = lending_club[1:7, num_pred])
  xy_pred <- xy_fit$fit$family$linkinv(xy_pred)
  xy_pred <- tibble(bad = 1 - xy_pred, good = xy_pred)

  expect_equal(xy_pred, predict_classprob(xy_fit, lending_club[1:7, num_pred]))

  res_form <- fit(
    logistic_reg() %>%
      set_engine("stan", seed = 1333, chains = 1),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    control = ctrl
  )

  # form_pred <-
  #   predict(res_form$fit,
  #           newdata = lending_club[1:7, c("funded_amnt", "int_rate")])
  # form_pred <- xy_fit$fit$family$linkinv(form_pred)
  # form_pred <- tibble(bad = 1 - form_pred, good = form_pred)
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
    predict_classprob(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]) %>%
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

  # stan_post <-
  #   posterior_linpred(res_form$fit, newdata = lending_club[1:5, ], seed = 13,
  #                     prob = 0.93, transform = TRUE)
  #
  # stan_lower <- apply(stan_post, 2, quantile, prob = 0.035)
  # stan_upper <- apply(stan_post, 2, quantile, prob = 0.965)
  # stan_std  <- apply(stan_post, 2, sd)

  stan_lower <-
    c(`1` = 0.913925483690233, `2` = 0.841801274737206, `3` = 0.91056642931229,
      `4` = 0.913619668586545, `5` = 0.987780279394871)
  stan_upper <-
    c(`1` = 0.978674663115785, `2` = 0.975178762720162, `3` = 0.984417491942267,
      `4` = 0.979606072215269, `5` = 0.9999049778978)
  stan_std <-
    c(`1` = 0.0181025303127182, `2` = 0.0388665155739319, `3` = 0.0205886091162274,
      `4` = 0.0181715224502082, `5` = 0.00405145389896896)

  expect_equivalent(confidence_parsnip$.pred_lower, stan_lower)
  expect_equivalent(confidence_parsnip$.pred_upper, stan_upper)
  expect_equivalent(confidence_parsnip$.std_error, stan_std)

  # stan_pred_post <-
  #   posterior_predict(res_form$fit, newdata = lending_club[1:5, ], seed = 13,
  #                     prob = 0.93)
  #
  # stan_pred_lower <- apply(stan_pred_post, 2, quantile, prob = 0.035)
  # stan_pred_upper <- apply(stan_pred_post, 2, quantile, prob = 0.965)
  # stan_pred_std  <- apply(stan_pred_post, 2, sd)

  stan_pred_lower <- c(`1` = 0, `2` = 0, `3` = 0, `4` = 0, `5` = 1)
  stan_pred_upper <- c(`1` = 1, `2` = 1, `3` = 1, `4` = 1, `5` = 1)
  stan_pred_std  <-
    c(`1` = 0.211744742168102, `2` = 0.265130711714607, `3` = 0.209589904165081,
      `4` = 0.198389410902796, `5` = 0.0446989708829856)
  expect_equivalent(prediction_parsnip$.pred_lower, stan_pred_lower)
  expect_equivalent(prediction_parsnip$.pred_upper, stan_pred_upper)
  expect_equivalent(prediction_parsnip$.std_error, stan_pred_std, tolerance = 0.1)
})



