library(testthat)
library(parsnip)
library(dplyr)
library(rlang)

# ------------------------------------------------------------------------------

context("prediciton with failed models")

# ------------------------------------------------------------------------------

iris_bad <-
  iris %>%
  mutate(big_num = Inf)

data("lending_club")

lending_club <-
  lending_club %>%
  slice(1:200) %>%
  mutate(big_num = Inf)

lvl <- levels(lending_club$Class)

# ------------------------------------------------------------------------------

ctrl <- fit_control(catch = TRUE)

# ------------------------------------------------------------------------------

test_that('numeric model', {
  lm_mod <-
    linear_reg() %>%
    set_engine("lm") %>%
    fit(Sepal.Length ~ ., data = iris_bad, control = ctrl)

  num_res <- predict(lm_mod, iris_bad[1:11, -1])
  expect_equal(num_res, tibble(.pred = rep(NA_real_, 11)))

  exp_int_res <- tibble(.pred_lower = rep(NA_real_, 11), .pred_upper = rep(NA_real_, 11))
  ci_res <- predict(lm_mod, iris_bad[1:11, -1], type = "conf_int")
  expect_equal(ci_res, exp_int_res)

  pi_res <- predict(lm_mod, iris_bad[1:11, -1], type = "pred_int")
  expect_equal(pi_res, exp_int_res)

})

# ------------------------------------------------------------------------------

test_that('classification model', {
  log_reg <-
    logistic_reg() %>%
    set_engine("glm") %>%
    fit(Class ~ log(funded_amnt) + int_rate + big_num, data = lending_club, control = ctrl)

  cls_res <- predict(log_reg, lending_club %>% slice(1:7) %>% dplyr::select(-Class))
  exp_cls_res <- tibble(.pred_class = factor(rep(NA_character_, 7), levels = lvl))
  expect_equal(cls_res, exp_cls_res)

  prb_res <-
    predict(log_reg, lending_club %>% slice(1:7) %>% dplyr::select(-Class), type = "prob")
  exp_prb_res <- tibble(.pred_bad = rep(NA_real_, 7), .pred_good = rep(NA_real_, 7))
  expect_equal(prb_res, exp_prb_res)

  ci_res <-
    predict(log_reg, lending_club %>% slice(1:7) %>% dplyr::select(-Class), type = "conf_int")
  exp_ci_res <-
    tibble(
      .pred_bad_lower = rep(NA_real_, 7),
      .pred_bad_upper = rep(NA_real_, 7),
      .pred_good_lower = rep(NA_real_, 7),
      .pred_good_upper = rep(NA_real_, 7)
      )
  expect_equal(ci_res, exp_ci_res)
})

