library(testthat)
context("mars tests")
library(parsnip)
library(rlang)

test_that('primary arguments', {
  basic <- mars(mode = "regression")
  basic_mars <- translate(basic, engine = "earth")
  expect_equal(basic_mars$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg())
               )
  )

  num_terms <- mars(num_terms = 4, mode = "classification")
  num_terms_mars <- translate(num_terms, engine = "earth")
  expect_equal(num_terms_mars$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 nprune = 4,
                 glm = quote(list(family = stats::binomial))
               )
  )

  prod_degree <- mars(prod_degree = 1, mode = "regression")
  prod_degree_mars <- translate(prod_degree, engine = "earth")
  expect_equal(prod_degree_mars$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 degree = 1
               )
  )

  prune_method_v <- mars(prune_method = varying(), mode = "regression")
  prune_method_v_mars <- translate(prune_method_v, engine = "earth")
  expect_equal(prune_method_v_mars$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 pmethod = varying()
               )
  )
})

test_that('engine arguments', {
  mars_keep <- mars(mode = "regression", others = list(keepxy = FALSE))
  expect_equal(translate(mars_keep, engine = "earth")$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 keepxy = FALSE
               )
  )
})


test_that('updating', {
  expr1     <- mars(               others = list(model = FALSE))
  expr1_exp <- mars(num_terms = 1, others = list(model = FALSE))

  expr2     <- mars(num_terms = varying())
  expr2_exp <- mars(num_terms = varying(), others = list(nk = 10))

  expr3     <- mars(num_terms = 1, prod_degree = varying())
  expr3_exp <- mars(num_terms = 1)

  expr4     <- mars(num_terms = 0, others = list(nk = 10))
  expr4_exp <- mars(num_terms = 0, others = list(nk = 10, trace = 2))

  expr5     <- mars(num_terms = 1, others = list(nk = 10))
  expr5_exp <- mars(num_terms = 1, others = list(nk = 10, trace = 2))

  expect_equal(update(expr1, num_terms = 1), expr1_exp)
  expect_equal(update(expr2, others = list(nk = 10)), expr2_exp)
  expect_equal(update(expr3, num_terms = 1, fresh = TRUE), expr3_exp)
  expect_equal(update(expr4, others = list(trace = 2)), expr4_exp)
  expect_equal(update(expr5, others = list(nk = 10, trace = 2)), expr5_exp)

})

test_that('bad input', {
  expect_error(mars(prod_degree = -1))
  expect_error(mars(num_terms = -1))
  expect_error(translate(mars(), engine = "wat?"))
  expect_warning(translate(mars(mode = "regression"), engine = NULL))
  expect_error(translate(mars(formula = y ~ x)))
  expect_warning(
    translate(
      mars(mode = "regression", others = list(x = iris[,1:3], y = iris$Species)),
      engine = "earth")
  )
})

###################################################################

num_pred <- c("Sepal.Width", "Petal.Width", "Petal.Length")
iris_bad_form <- as.formula(Species ~ term)
iris_basic <- mars(mode = "regression")
ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

test_that('mars execution', {

  skip_if_not_installed("earth")

  expect_error(
    res <- fit(
      iris_basic,
      Sepal.Length ~ log(Sepal.Width) + Species,
      data = iris,
      control = ctrl,
      engine = "earth"
    ),
    regexp = NA
  )
  expect_error(
    res <- fit_xy(
      iris_basic,
      x = iris[, num_pred],
      y = iris$Sepal.Length,
      engine = "earth",
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    res <- fit(
      iris_basic,
      iris_bad_form,
      data = iris,
      engine = "earth",
      control = ctrl
    )
  )

  ## multivariate y

  expect_error(
    res <- fit(
      iris_basic,
      cbind(Sepal.Width, Petal.Width) ~ .,
      data = iris,
      control = ctrl,
      engine = "earth"
    ),
    regexp = NA
  )

})

