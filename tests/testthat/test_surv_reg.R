library(testthat)
library(parsnip)
library(rlang)
library(survival)

# ------------------------------------------------------------------------------

context("parametric survival models")
source("helpers.R")

# ------------------------------------------------------------------------------

test_that('primary arguments', {
  basic <- surv_reg()
  basic_flexsurv <- translate(basic, engine = "flexsurv")

  expect_equal(basic_flexsurv$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg())
               )
  )

  normal <- surv_reg(dist = "lnorm")
  normal_flexsurv <- translate(normal, engine = "flexsurv")
  expect_equal(normal_flexsurv$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 dist = new_empty_quosure("lnorm")
               )
  )

  dist_v <- surv_reg(dist = varying())
  dist_v_flexsurv <- translate(dist_v, engine = "flexsurv")
  expect_equal(dist_v_flexsurv$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 dist = new_empty_quosure(varying())
               )
  )
})

test_that('engine arguments', {
  fs_cl <- surv_reg(cl = .99)
  expect_equal(translate(fs_cl, engine = "flexsurv")$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 cl = new_empty_quosure(.99)
               )
  )

})


test_that('updating', {
  expr1     <- surv_reg(                cl = .99)
  expr1_exp <- surv_reg(dist = "lnorm", cl = .99)

  expr2     <- surv_reg(dist = varying())
  expr2_exp <- surv_reg(dist = varying(), cl = .99)

  expect_equal(update(expr1, dist = "lnorm"), expr1_exp)
  expect_equal(update(expr2, cl = .99), expr2_exp)
})

test_that('bad input', {
  expect_error(surv_reg(mode = "classification"))
  expect_error(translate(surv_reg(), engine = "wat?"))
  expect_warning(translate(surv_reg(), engine = NULL))
  expect_error(translate(surv_reg(formula = y ~ x)))
  expect_warning(translate(surv_reg(formula = y ~ x), engine = "flexsurv"))
})
