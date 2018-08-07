library(testthat)
context("fit interfaces")
library(parsnip)
library(rlang)

f <- y ~ x

smod <- surv_reg()
rmod <- linear_reg()

sprk <- 1:10
class(sprk) <- c(class(sprk), "tbl_spark")

tester <-
  function(object, formula = NULL,  data = NULL, model)
    parsnip:::check_interface(formula, data, match.call(expand.dots = TRUE), model)
tester_xy <-
  function(object, x = NULL, y = NULL, model)
    parsnip:::check_xy_interface(x, y, match.call(expand.dots = TRUE), model)


test_that('good args', {
  expect_equal(   tester(NULL, formula = f, data = iris, model = rmod), "formula")
  expect_equal(tester_xy(NULL, x = iris, y = iris, model = rmod), "data.frame")
  expect_equal(   tester(NULL, f, data = iris, model = rmod), "formula")
  expect_equal(   tester(NULL, f, data = sprk, model = rmod), "formula")  
})

#test_that('unnamed args', {
#  expect_error(tester(NULL, iris, y = iris, model = rmod))
#  expect_error(tester(NULL, data = iris, model = rmod))
#})
#
test_that('wrong args', {
 expect_error(tester_xy(NULL, x = sprk, y = iris, model = rmod))
 expect_error(tester_xy(NULL, x = iris, y = iris$Sepal.Length, model = smod))
 expect_error(tester(NULL, f,  data = as.matrix(iris[, 1:4])))
})

