library(testthat)
context("multinom regression")
library(parsnip)
library(rlang)

test_that('primary arguments', {
  basic <- multinom_reg()
  basic_glmnet <- translate(basic, engine = "glmnet")
  expect_equal(basic_glmnet$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 family = "multinomial"
               )
  )

  mixture <- multinom_reg(mixture = 0.128)
  mixture_glmnet <- translate(mixture, engine = "glmnet")
  expect_equal(mixture_glmnet$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 alpha = 0.128,
                 family = "multinomial"
               )
  )

  penalty <- multinom_reg(penalty = 1)
  penalty_glmnet <- translate(penalty, engine = "glmnet")
  expect_equal(penalty_glmnet$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 lambda = 1,
                 family = "multinomial"
               )
  )

  mixture_v <- multinom_reg(mixture = varying())
  mixture_v_glmnet <- translate(mixture_v, engine = "glmnet")
  expect_equal(mixture_v_glmnet$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 alpha = varying(),
                 family = "multinomial"
               )
  )

})

test_that('engine arguments', {
  glmnet_nlam <- multinom_reg(others = list(nlambda = 10))
  expect_equal(translate(glmnet_nlam, engine = "glmnet")$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 nlambda = 10,
                 family = "multinomial"
               )
  )

})


test_that('updating', {
  expr1     <- multinom_reg(             others = list(intercept = TRUE))
  expr1_exp <- multinom_reg(mixture = 0, others = list(intercept = TRUE))

  expr2     <- multinom_reg(mixture = varying())
  expr2_exp <- multinom_reg(mixture = varying(), others = list(nlambda = 10))

  expr3     <- multinom_reg(mixture = 0, penalty = varying())
  expr3_exp <- multinom_reg(mixture = 1)

  expr4     <- multinom_reg(mixture = 0, others = list(nlambda = 10))
  expr4_exp <- multinom_reg(mixture = 0, others = list(nlambda = 10, pmax = 2))

  expr5     <- multinom_reg(mixture = 1, others = list(nlambda = 10))
  expr5_exp <- multinom_reg(mixture = 1, others = list(nlambda = 10, pmax = 2))

  expect_equal(update(expr1, mixture = 0), expr1_exp)
  expect_equal(update(expr2, others = list(nlambda = 10)), expr2_exp)
  expect_equal(update(expr3, mixture = 1, fresh = TRUE), expr3_exp)
  expect_equal(update(expr4, others = list(pmax = 2)), expr4_exp)
  expect_equal(update(expr5, others = list(nlambda = 10, pmax = 2)), expr5_exp)

})

test_that('bad input', {
  expect_error(multinom_reg(ase.weights = var))
  expect_error(multinom_reg(mode = "regression"))
  expect_error(multinom_reg(penalty = -1))
  expect_error(multinom_reg(mixture = -1))
  expect_error(translate(multinom_reg(), engine = "wat?"))
  expect_warning(translate(multinom_reg(), engine = NULL))
  expect_error(translate(multinom_reg(formula = y ~ x)))
  expect_warning(translate(multinom_reg(others = list(x = iris[,1:3], y = iris$Species)), engine = "glmnet"))
})
