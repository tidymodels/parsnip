library(testthat)
library(parsnip)
library(recipes)
library(rlang)

rec <- recipe(~ ., data = iris)
f <- y ~ x

smod <- surv_reg()
rmod <- linear_reg()

tester <-
  function(object, formula = NULL, recipe = NULL, x = NULL, y = NULL, data = NULL, model)
    parsnip:::check_interface(formula, recipe, x, y, data, match.call(expand.dots = TRUE), model)

test_that('good args', {
  expect_equal(tester(NULL, formula = f, data = iris, model = rmod), "formula")
  expect_equal(tester(NULL, recipe = rec, data = iris, model = rmod), "recipe")
  expect_equal(tester(NULL, x = iris, y = iris, model = rmod), "data.frame")
  expect_equal(tester(NULL, f, data = iris, model = rmod), "formula")
  expect_equal(tester(NULL, formula = f, data = iris, y = iris, model = rmod), "formula")
})

test_that('unnamed args', {
  expect_error(tester(NULL, rec, data = iris, model = rmod))
  expect_error(tester(NULL, iris, y = iris, model = rmod))
  expect_error(tester(NULL, data = iris, model = rmod))
})

test_that('wrong args', {
  expect_error(tester(NULL, x = iris, data = iris, model = rmod))
  expect_error(tester(NULL, x = iris, y = iris$Sepal.Length, model = smod))
  expect_error(tester(NULL, f,  x = iris, y = iris, data = iris))
})

