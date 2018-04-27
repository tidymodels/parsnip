library(testthat)
library(parsnip)
library(recipes)
library(rlang)
library(flexsurv)


test_that('primary arguments', {
  basic <- surv_reg()
  basic_flexsurv <- translate(basic, engine = "flexsurv")

  expect_equal(basic_flexsurv$method$fit_args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 dist = "weibull"
               )
  )

  normal <- surv_reg(dist = "lnorm")
  normal_flexsurv <- translate(normal, engine = "flexsurv")
  expect_equal(normal_flexsurv$method$fit_args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 dist = "lnorm"
               )
  )

  dist_v <- surv_reg(dist = varying())
  dist_v_flexsurv <- translate(dist_v, engine = "flexsurv")
  expect_equal(dist_v_flexsurv$method$fit_args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 dist = varying()
               )
  )
})

test_that('engine arguments', {
  fs_cl <- surv_reg(others = list(cl = .99))
  expect_equal(translate(fs_cl, engine = "flexsurv")$method$fit_args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 cl = .99,
                 dist = "weibull"
               )
  )

})


test_that('updating', {
  expr1     <- surv_reg(                others = list(cl = .99))
  expr1_exp <- surv_reg(dist = "lnorm", others = list(cl = .99))

  expr2     <- surv_reg(dist = varying())
  expr2_exp <- surv_reg(dist = varying(), others = list(cl = .99))

  expect_equal(update(expr1, dist = "lnorm"), expr1_exp)
  expect_equal(update(expr2, others = list(cl = .99)), expr2_exp)
})

test_that('bad input', {
  expect_error(surv_reg(ase.weights = var))
  expect_error(surv_reg(mode = "classification"))
  expect_error(translate(surv_reg(), engine = "wat?"))
  expect_warning(translate(surv_reg(), engine = NULL))
  expect_error(translate(surv_reg(formula = y ~ x)))
  expect_warning(translate(surv_reg(others = list(formula = y ~ x)), engine = "flexsurv"))
})

###################################################################

data(bc)

set.seed(4566)
bc$group2 <- bc$group

basic_form <- Surv(recyrs, censrec) ~ group
complete_form <- Surv(recyrs) ~ group

basic_rec <- recipe(recyrs ~ ., data = bc) %>%
  add_role(censrec, new_role = "censoring var") %>%
  step_rm(rectime)

no_cens_rec <- recipe(recyrs ~ ., data = bc) %>%
  step_rm(rectime, censrec)

extra_param_rec <- recipe(recyrs ~ ., data = bc) %>%
  add_role(censrec, new_role = "censoring var") %>%
  add_role(group2, new_role = "shape") %>%
  step_rm(rectime)

surv_basic <- surv_reg()
ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

test_that('flexsurv execution', {
  skip_on_cran()

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
    res <- fit(
      surv_basic,
      x = bc[, "group", drop = FALSE],
      y = bc$recyrs,
      engine = "flexsurv",
      control = ctrl
    )
  )
  expect_error(
    res <- fit(
      surv_basic,
      recipe = basic_rec,
      data = bc,
      engine = "flexsurv",
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- fit(
      surv_basic,
      recipe = no_cens_rec,
      data = bc,
      engine = "flexsurv",
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- fit(
      surv_basic,
      recipe = extra_param_rec,
      data = bc,
      engine = "flexsurv",
      control = ctrl
    ),
    regexp = NA
  )
})
