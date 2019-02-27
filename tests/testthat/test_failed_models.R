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
  dplyr::slice(1:200) %>%
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

  expect_warning(num_res <- predict(lm_mod, iris_bad[1:11, -1]))
  expect_equal(num_res, NULL)

  expect_warning(ci_res <- predict(lm_mod, iris_bad[1:11, -1], type = "conf_int"))
  expect_equal(ci_res, NULL)

  expect_warning(pi_res <- predict(lm_mod, iris_bad[1:11, -1], type = "pred_int"))
  expect_equal(pi_res, NULL)

})

# ------------------------------------------------------------------------------

test_that('classification model', {
  log_reg <-
    logistic_reg() %>%
    set_engine("glm") %>%
    fit(Class ~ log(funded_amnt) + int_rate + big_num, data = lending_club, control = ctrl)

  expect_warning(
    cls_res <-
      predict(log_reg, lending_club %>%  dplyr::slice(1:7) %>% dplyr::select(-Class))
  )
  expect_equal(cls_res, NULL)

  expect_warning(
    prb_res <-
      predict(log_reg, lending_club %>%  dplyr::slice(1:7) %>% dplyr::select(-Class), type = "prob")
  )
  expect_equal(prb_res, NULL)

  expect_warning(
    ci_res <-
      predict(log_reg, lending_club %>%  dplyr::slice(1:7) %>% dplyr::select(-Class), type = "conf_int")
  )
  expect_equal(ci_res, NULL)
})

