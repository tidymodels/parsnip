library(testthat)
library(dplyr)
library(parsnip)

context("setting modes works as intended")

test_that("correct modes of boost_tree",{
  basic <- boost_tree()
  basic_expect <- basic
  #default
  expect_equal(basic$mode, "unknown")
  #set classification
  basic_expect$mode <- "classification"
  expect_equal(basic %>% set_mode("classification"), basic_expect)
  #set regression
  basic_expect$mode <- "regression"
  expect_equal(basic %>% set_mode("regression"), basic_expect)
  #attempt to set incorrect
  expect_error(basic %>% set_mode("censored regression"))
})

test_that("correct modes of decision_tree",{
  basic <- decision_tree()
  basic_expect <- basic
  #default
  expect_equal(basic$mode, "unknown")
  #set classification
  basic_expect$mode <- "classification"
  expect_equal(basic %>% set_mode("classification"), basic_expect)
  #set regression
  basic_expect$mode <- "regression"
  expect_equal(basic %>% set_mode("regression"), basic_expect)
  #attempt to set incorrect
  expect_error(basic %>% set_mode("censored regression"))
})

test_that("correct modes of linear_reg", {
  basic <- linear_reg()
  expect_equal(basic$mode, "regression")
  expect_error(linear_reg("classification"))
  expect_error(basic %>% set_mode("classification"))
})

test_that("correct modes of logistic_reg", {
  basic <- logistic_reg()
  expect_equal(basic$mode, "classification")
  expect_error(logistic_reg("regression"))
  expect_error(basic %>% set_mode("regression"))
})

test_that("correct modes of mars",{
  basic <- mars()
  basic_expect <- basic
  #default
  expect_equal(basic$mode, "unknown")
  #set classification
  basic_expect$mode <- "classification"
  expect_equal(basic %>% set_mode("classification"), basic_expect)
  #set regression
  basic_expect$mode <- "regression"
  expect_equal(basic %>% set_mode("regression"), basic_expect)
  #attempt to set incorrect
  expect_error(basic %>% set_mode("censored regression"))
})

test_that("correct modes of mlp",{
  basic <- mlp()
  basic_expect <- basic
  #default
  expect_equal(basic$mode, "unknown")
  #set classification
  basic_expect$mode <- "classification"
  expect_equal(basic %>% set_mode("classification"), basic_expect)
  #set regression
  basic_expect$mode <- "regression"
  expect_equal(basic %>% set_mode("regression"), basic_expect)
  #attempt to set incorrect
  expect_error(basic %>% set_mode("censored regression"))
})

test_that("correct modes of multinom_reg", {
  basic <- multinom_reg()
  expect_equal(basic$mode, "classification")
  expect_error(multinom_reg("regression"))
  expect_error(basic %>% set_mode("regression"))
})

test_that("correct modes of nearest_neighbor",{
  basic <- nearest_neighbor()
  basic_expect <- basic
  #default
  expect_equal(basic$mode, "unknown")
  #set classification
  basic_expect$mode <- "classification"
  expect_equal(basic %>% set_mode("classification"), basic_expect)
  #set regression
  basic_expect$mode <- "regression"
  expect_equal(basic %>% set_mode("regression"), basic_expect)
  #attempt to set incorrect
  expect_error(basic %>% set_mode("censored regression"))
})

test_that("correct modes of null_model",{
  basic <- null_model()
  basic_expect <- basic
  #default
  expect_equal(basic$mode, "unknown")
  #set classification
  basic_expect$mode <- "classification"
  expect_equal(basic %>% set_mode("classification"), basic_expect)
  #set regression
  basic_expect$mode <- "regression"
  expect_equal(basic %>% set_mode("regression"), basic_expect)
  #attempt to set incorrect
  expect_error(basic %>% set_mode("censored regression"))
})

test_that("correct modes of rand_forest",{
  basic <- rand_forest()
  basic_expect <- basic
  #default
  expect_equal(basic$mode, "unknown")
  #set classification
  basic_expect$mode <- "classification"
  expect_equal(basic %>% set_mode("classification"), basic_expect)
  #set regression
  basic_expect$mode <- "regression"
  expect_equal(basic %>% set_mode("regression"), basic_expect)
  #attempt to set incorrect
  expect_error(basic %>% set_mode("censored regression"))
})

test_that("correct modes of survival_reg", {
  basic <- survival_reg()
  expect_equal(basic$mode, "censored regression")
  expect_error(surv_reg("classification"))
  expect_error(basic %>% set_mode("classification"))
})

test_that("correct modes of svm_poly",{
  basic <- svm_poly()
  basic_expect <- basic
  #default
  expect_equal(basic$mode, "unknown")
  #set classification
  basic_expect$mode <- "classification"
  expect_equal(basic %>% set_mode("classification"), basic_expect)
  #set regression
  basic_expect$mode <- "regression"
  expect_equal(basic %>% set_mode("regression"), basic_expect)
  #attempt to set incorrect
  expect_error(basic %>% set_mode("censored regression"))
})

test_that("correct modes of svm_rbf",{
  basic <- svm_rbf()
  basic_expect <- basic
  #default
  expect_equal(basic$mode, "unknown")
  #set classification
  basic_expect$mode <- "classification"
  expect_equal(basic %>% set_mode("classification"), basic_expect)
  #set regression
  basic_expect$mode <- "regression"
  expect_equal(basic %>% set_mode("regression"), basic_expect)
  #attempt to set incorrect
  expect_error(basic %>% set_mode("censored regression"))
})
