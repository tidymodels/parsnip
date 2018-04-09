library(testthat)
library(parsnip)
library(recipes)
library(rlang)

rec <- recipe(~ ., data = iris)
f <- y ~ x

tester <-
  function(object, formula = NULL, recipe = NULL, x = NULL, y = NULL, data = NULL)
    parsnip:::check_interface(formula, recipe, x, y, data, match.call(expand.dots = TRUE))

test_that('good args', {
  expect_equal(tester(NULL, formula = f, data = iris), "formula")
  expect_equal(tester(NULL, recipe = rec, data = iris), "recipe")
  expect_equal(tester(NULL, x = iris, y = iris), "data.frame")
  expect_equal(tester(NULL, f, data = iris), "formula")
  expect_equal(tester(NULL, formula = f, data = iris, y = iris), "formula")
})

test_that('unnamed args', {
  expect_error(tester(NULL, rec, data = iris))
  expect_error(tester(NULL, iris, y = iris))
  expect_error(tester(NULL, data = iris))
})

test_that('wrong args', {
  expect_error(tester(NULL, x = iris, data = iris))
  expect_error(tester(NULL, f,  x = iris, y = iris, data = iris))
})

