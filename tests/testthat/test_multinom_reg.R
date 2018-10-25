library(testthat)
library(parsnip)
library(rlang)

# ------------------------------------------------------------------------------

context("multinom regression")
source("helpers.R")

# ------------------------------------------------------------------------------

test_that('primary arguments', {
  basic <- multinom_reg()
  basic_glmnet <- translate(basic %>% set_engine("glmnet"))
  expect_equal(basic_glmnet$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 family = "multinomial"
               )
  )

  mixture <- multinom_reg(mixture = 0.128)
  mixture_glmnet <- translate(mixture %>% set_engine("glmnet"))
  expect_equal(mixture_glmnet$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 alpha = new_empty_quosure(0.128),
                 family = "multinomial"
               )
  )

  penalty <- multinom_reg(penalty = 1)
  penalty_glmnet <- translate(penalty %>% set_engine("glmnet"))
  expect_equal(penalty_glmnet$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 lambda = new_empty_quosure(1),
                 family = "multinomial"
               )
  )

  mixture_v <- multinom_reg(mixture = varying())
  mixture_v_glmnet <- translate(mixture_v %>% set_engine("glmnet"))
  expect_equal(mixture_v_glmnet$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 alpha = new_empty_quosure(varying()),
                 family = "multinomial"
               )
  )

})

test_that('engine arguments', {
  glmnet_nlam <- multinom_reg()
  expect_equal(
    translate(glmnet_nlam %>% set_engine("glmnet", nlambda = 10))$method$fit$args,
    list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      weights = expr(missing_arg()),
      nlambda = new_empty_quosure(10),
      family = "multinomial"
    )
  )

})


test_that('updating', {
  expr1     <- multinom_reg() %>% set_engine("glmnet", intercept = TRUE)
  expr1_exp <- multinom_reg(mixture = 0) %>% set_engine("glmnet", intercept = TRUE)

  expr2     <- multinom_reg(mixture = varying()) %>% set_engine("glmnet")
  expr2_exp <- multinom_reg(mixture = varying()) %>% set_engine("glmnet", nlambda = 10)

  expr3     <- multinom_reg(mixture = 0, penalty = varying()) %>% set_engine("glmnet")
  expr3_exp <- multinom_reg(mixture = 1) %>% set_engine("glmnet")

  expr4     <- multinom_reg(mixture = 0) %>% set_engine("glmnet", nlambda = 10)
  expr4_exp <- multinom_reg(mixture = 0) %>% set_engine("glmnet", nlambda = 10, pmax = 2)

  expr5     <- multinom_reg(mixture = 1) %>% set_engine("glmnet", nlambda = 10)
  expr5_exp <- multinom_reg(mixture = 1) %>% set_engine("glmnet", nlambda = 10, pmax = 2)

  # expect_equal(update(expr1 %>% set_engine("glmnet"), mixture = 0), expr1_exp)
  expect_equal(update(expr2) %>% set_engine("glmnet", nlambda = 10), expr2_exp)
  expect_equal(update(expr3, mixture = 1, fresh = TRUE) %>% set_engine("glmnet"), expr3_exp)
  # expect_equal(update(expr4 %>% set_engine("glmnet", pmax = 2)), expr4_exp)
  expect_equal(update(expr5) %>% set_engine("glmnet", nlambda = 10, pmax = 2), expr5_exp)

})

test_that('bad input', {
  expect_error(multinom_reg(mode = "regression"))
  expect_error(translate(multinom_reg() %>% set_engine("wat?")))
  expect_error(translate(multinom_reg() %>% set_engine()))
  expect_warning(translate(multinom_reg() %>% set_engine("glmnet", x = iris[,1:3], y = iris$Species)))
})
