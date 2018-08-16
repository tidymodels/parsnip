library(testthat)
context("check predict output structures")
library(parsnip)
library(tibble)

lm_fit <- 
  linear_reg(mode = "regression") %>%
  fit(Sepal.Length ~ ., data = iris, engine = "lm")

test_that('regression predictions', {
  expect_true(is_tibble(predict(lm_fit, newdata = iris[1:5,-1])))
  expect_true(is.vector(predict_num(lm_fit, newdata = iris[1:5,-1])))
  expect_equal(names(predict(lm_fit, newdata = iris[1:5,-1])), ".pred")
})

class_dat <- airquality[complete.cases(airquality),]
class_dat$Ozone <- factor(ifelse(class_dat$Ozone >= 31, "high", "low"))

lr_fit <- 
  logistic_reg() %>%
  fit(Ozone ~ ., data = class_dat, engine = "glm")

test_that('classification predictions', {
  expect_true(is_tibble(predict(lr_fit, newdata = class_dat[1:5,-1])))
  expect_true(is.factor(predict_class(lr_fit, newdata = class_dat[1:5,-1])))
  expect_equal(names(predict(lr_fit, newdata = class_dat[1:5,-1])), ".pred_class")
  
  expect_true(is_tibble(predict(lr_fit, newdata = class_dat[1:5,-1], type = "prob")))
  expect_true(is_tibble(predict_classprob(lr_fit, newdata = class_dat[1:5,-1])))
  expect_equal(names(predict(lr_fit, newdata = class_dat[1:5,-1], type = "prob")), 
               c(".pred_high", ".pred_low"))
})

class_dat2 <- airquality[complete.cases(airquality),]
class_dat2$Ozone <- factor(ifelse(class_dat2$Ozone >= 31, "high+values", "2low"))

lr_fit_2 <- 
  logistic_reg() %>%
  fit(Ozone ~ ., data = class_dat2, engine = "glm")

test_that('non-standard levels', {
  expect_true(is_tibble(predict(lr_fit, newdata = class_dat[1:5,-1])))
  expect_true(is.factor(predict_class(lr_fit, newdata = class_dat[1:5,-1])))
  expect_equal(names(predict(lr_fit, newdata = class_dat[1:5,-1])), ".pred_class")
  
  expect_true(is_tibble(predict(lr_fit_2, newdata = class_dat2[1:5,-1], type = "prob")))
  expect_true(is_tibble(predict_classprob(lr_fit_2, newdata = class_dat2[1:5,-1])))
  expect_equal(names(predict(lr_fit_2, newdata = class_dat2[1:5,-1], type = "prob")), 
               c(".pred_2low", ".pred_high+values"))
  expect_equal(names(predict_classprob(lr_fit_2, newdata = class_dat2[1:5,-1])), 
               c("2low", "high+values"))
})
