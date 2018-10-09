library(testthat)
library(parsnip)
library(dplyr)

context("changing arguments and engine")

test_that('pipe arguments', {
  mod_1 <- rand_forest() %>%
    set_args(mtry = 1, something = "blah")
  expect_equal(mod_1$args$mtry, 1)
  expect_equal(mod_1$others$something, "blah")
  
  mod_2 <- rand_forest(mtry = 2, others = list(var = "x")) %>%
    set_args(mtry = 1, something = "blah")
  expect_equal(mod_2$args$mtry, 1)
  expect_equal(mod_2$others$something, "blah")
  expect_equal(mod_2$others$var, "x") 
  
  expect_error(rand_forest() %>% set_args())
  
})


test_that('pipe engine', {
  mod_1 <- rand_forest() %>%
    set_mode("regression")
  expect_equal(mod_1$mode, "regression")
  
  expect_error(rand_forest() %>% set_mode())
  expect_error(rand_forest() %>% set_mode(2))
  expect_error(rand_forest() %>% set_mode("haberdashery"))
})