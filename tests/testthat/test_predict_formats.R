library(testthat)
library(parsnip)
library(tibble)

# ------------------------------------------------------------------------------

context("check predict output structures")

lm_fit <-
  linear_reg(mode = "regression") %>%
  set_engine("lm") %>%
  fit(Sepal.Length ~ ., data = iris)

class_dat <- airquality[complete.cases(airquality),]
class_dat$Ozone <- factor(ifelse(class_dat$Ozone >= 31, "high", "low"))

lr_fit <-
  logistic_reg() %>%
  set_engine("glm") %>%
  fit(Ozone ~ ., data = class_dat)

class_dat2 <- airquality[complete.cases(airquality),]
class_dat2$Ozone <- factor(ifelse(class_dat2$Ozone >= 31, "high+values", "2low"))

lr_fit_2 <-
  logistic_reg() %>%
  set_engine("glm") %>%
  fit(Ozone ~ ., data = class_dat2)

# ------------------------------------------------------------------------------

test_that('regression predictions', {
  expect_true(is_tibble(predict(lm_fit, new_data = iris[1:5,-1])))
  expect_true(is.vector(predict_numeric(lm_fit, new_data = iris[1:5,-1])))
  expect_equal(names(predict(lm_fit, new_data = iris[1:5,-1])), ".pred")
})

test_that('classification predictions', {
  expect_true(is_tibble(predict(lr_fit, new_data = class_dat[1:5,-1])))
  expect_true(is.factor(predict_class(lr_fit, new_data = class_dat[1:5,-1])))
  expect_equal(names(predict(lr_fit, new_data = class_dat[1:5,-1])), ".pred_class")

  expect_true(is_tibble(predict(lr_fit, new_data = class_dat[1:5,-1], type = "prob")))
  expect_true(is_tibble(predict_classprob(lr_fit, new_data = class_dat[1:5,-1])))
  expect_equal(names(predict(lr_fit, new_data = class_dat[1:5,-1], type = "prob")),
               c(".pred_high", ".pred_low"))
})

test_that('non-standard levels', {
  expect_true(is_tibble(predict(lr_fit, new_data = class_dat[1:5,-1])))
  expect_true(is.factor(predict_class(lr_fit, new_data = class_dat[1:5,-1])))
  expect_equal(names(predict(lr_fit, new_data = class_dat[1:5,-1])), ".pred_class")

  expect_true(is_tibble(predict(lr_fit_2, new_data = class_dat2[1:5,-1], type = "prob")))
  expect_true(is_tibble(predict_classprob(lr_fit_2, new_data = class_dat2[1:5,-1])))
  expect_equal(names(predict(lr_fit_2, new_data = class_dat2[1:5,-1], type = "prob")),
               c(".pred_2low", ".pred_high+values"))
  expect_equal(names(predict_classprob(lr_fit_2, new_data = class_dat2[1:5,-1])),
               c("2low", "high+values"))
})
