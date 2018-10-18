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

# ------------------------------------------------------------------------------

basic_form <- Surv(recyrs, censrec) ~ group
complete_form <- Surv(recyrs) ~ group

surv_basic <- surv_reg()
ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

test_that('flexsurv execution', {
  skip_if_not_installed("flexsurv")

  library(flexsurv)
  data(bc)

  set.seed(4566)
  bc$group2 <- bc$group

  # passes interactively but not on R CMD check
  expect_error(
    res <- fit(
      surv_basic,
      Surv(recyrs, censrec) ~ group,
      data = bc,
      control = ctrl,
      engine = "flexsurv"
    ),
    regexp = NA
  )
  expect_error(
    res <- fit(
      surv_basic,
      Surv(recyrs) ~ group,
      data = bc,
      control = ctrl,
      engine = "flexsurv"
    ),
    regexp = NA
  )
  expect_error(
    res <- fit_xy(
      surv_basic,
      x = bc[, "group", drop = FALSE],
      y = bc$recyrs,
      engine = "flexsurv",
      control = ctrl
    )
  )
})
