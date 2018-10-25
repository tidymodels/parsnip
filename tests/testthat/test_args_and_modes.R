library(testthat)
library(parsnip)
library(dplyr)
library(rlang)

context("changing arguments and engine")

test_that('pipe arguments', {
  mod_1 <- rand_forest() %>%
    set_args(mtry = 1)
  expect_equal(
    quo_get_expr(mod_1$args$mtry),
    1
  )
  expect_equal(
    quo_get_env(mod_1$args$mtry),
    empty_env()
  )

  mod_2 <- rand_forest(mtry = 2) %>%
    set_args(mtry = 1)

  var_env <- rlang::current_env()

  expect_equal(
    quo_get_expr(mod_2$args$mtry),
    1
  )
  expect_equal(
    quo_get_env(mod_2$args$mtry),
    empty_env()
  )

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
