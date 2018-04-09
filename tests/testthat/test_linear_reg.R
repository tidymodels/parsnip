library(testthat)
library(parsnip)
library(recipes)
library(rlang)

#TODO add spark test cases (in another file that is ignored on build?)

test_that('primary arguments', {
  basic <- linear_reg()
  basic_lm <- translate(basic, engine = "lm")
  basic_glmnet <- translate(basic, engine = "glmnet")
  basic_stan <- translate(basic, engine = "stan")
  basic_spark <- translate(basic, engine = "spark")
  expect_equal(basic_lm$method$fit_args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg())
               )
  )
  expect_equal(basic_glmnet$method$fit_args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 family = "gaussian"
               )
  )
  expect_equal(basic_stan$method$fit_args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 family = "gaussian"
               )
  )
  expect_equal(basic_spark$method$fit_args,
               list(
                 x = quote(missing_arg()),
                 formula = quote(missing_arg()),
                 weight_col = quote(missing_arg())
               )
  )

  mixture <- linear_reg(mixture = 0.128)
  mixture_glmnet <- translate(mixture, engine = "glmnet")
  mixture_spark <- translate(mixture, engine = "spark")
  expect_equal(mixture_glmnet$method$fit_args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 alpha = 0.128,
                 family = "gaussian"
               )
  )
  expect_equal(mixture_spark$method$fit_args,
               list(
                 x = quote(missing_arg()),
                 formula = quote(missing_arg()),
                 weight_col = quote(missing_arg()),
                 elastic_net_param = 0.128
               )
  )

  regularization <- linear_reg(regularization = 1)
  regularization_glmnet <- translate(regularization, engine = "glmnet")
  regularization_spark <- translate(regularization, engine = "spark")
  expect_equal(regularization_glmnet$method$fit_args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 lambda = 1,
                 family = "gaussian"
               )
  )
  expect_equal(regularization_spark$method$fit_args,
               list(
                 x = quote(missing_arg()),
                 formula = quote(missing_arg()),
                 weight_col = quote(missing_arg()),
                 reg_param = 1
               )
  )

  mixture_v <- linear_reg(mixture = varying())
  mixture_v_glmnet <- translate(mixture_v, engine = "glmnet")
  mixture_v_spark <- translate(mixture_v, engine = "spark")
  expect_equal(mixture_v_glmnet$method$fit_args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 alpha = varying(),
                 family = "gaussian"
               )
  )
  expect_equal(mixture_v_spark$method$fit_args,
               list(
                 x = quote(missing_arg()),
                 formula = quote(missing_arg()),
                 weight_col = quote(missing_arg()),
                 elastic_net_param = varying()
               )
  )

})

test_that('engine arguments', {
  lm_fam <- linear_reg(others = list(model = FALSE))
  expect_equal(translate(lm_fam, engine = "lm")$method$fit_args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 model = FALSE
               )
  )

  glmnet_nlam <- linear_reg(others = list(nlambda = 10))
  expect_equal(translate(glmnet_nlam, engine = "glmnet")$method$fit_args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 nlambda = 10,
                 family = "gaussian"
               )
  )

  stan_samp <- linear_reg(others = list(chains = 1, iter = 5))
  expect_equal(translate(stan_samp, engine = "stan")$method$fit_args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 chains = 1,
                 iter = 5,
                 family = "gaussian"
               )
  )

  spark_iter <- linear_reg(others = list(max_iter = 20))
  expect_equal(translate(spark_iter, engine = "spark")$method$fit_args,
               list(
                 x = quote(missing_arg()),
                 formula = quote(missing_arg()),
                 weight_col = quote(missing_arg()),
                 max_iter = 20
               )
  )

})


test_that('updating', {
  expr1     <- linear_reg(             others = list(model = FALSE))
  expr1_exp <- linear_reg(mixture = 0, others = list(model = FALSE))

  expr2     <- linear_reg(mixture = varying())
  expr2_exp <- linear_reg(mixture = varying(), others = list(nlambda = 10))

  expr3     <- linear_reg(mixture = 0, regularization = varying())
  expr3_exp <- linear_reg(mixture = 1)

  expr4     <- linear_reg(mixture = 0, others = list(nlambda = 10))
  expr4_exp <- linear_reg(mixture = 0, others = list(nlambda = 10, pmax = 2))

  expr5     <- linear_reg(mixture = 1, others = list(nlambda = 10))
  expr5_exp <- linear_reg(mixture = 1, others = list(nlambda = 10, pmax = 2))

  expect_equal(update(expr1, mixture = 0), expr1_exp)
  expect_equal(update(expr2, others = list(nlambda = 10)), expr2_exp)
  expect_equal(update(expr3, mixture = 1, fresh = TRUE), expr3_exp)
  expect_equal(update(expr4, others = list(pmax = 2)), expr4_exp)
  expect_equal(update(expr5, others = list(nlambda = 10, pmax = 2)), expr5_exp)

})

test_that('bad input', {
  expect_error(linear_reg(ase.weights = var))
  expect_error(linear_reg(mode = "classification"))
  expect_error(linear_reg(regularization = -1))
  expect_error(linear_reg(mixture = -1))
  expect_error(translate(linear_reg(), engine = "wat?"))
  expect_warning(translate(linear_reg(), engine = NULL))
  expect_error(translate(linear_reg(formula = y ~ x)))
  expect_warning(translate(linear_reg(others = list(x = iris[,1:3], y = iris$Species)), engine = "glmnet"))
  expect_warning(translate(linear_reg(others = list(formula = y ~ x)), engine = "lm"))
})

###################################################################


iris_form <- as.formula(Sepal.Width ~ log(Sepal.Length) + Species)
num_pred <- c("Sepal.Width", "Petal.Width", "Petal.Width")
iris_bad_form <- as.formula(Species ~ term)
iris_basic <- linear_reg()
ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)
iris_rec <- recipe(Sepal.Length ~ ., data = iris) %>%
  step_dummy(Species)
bad_rec <- recipe(Species ~ ., data = iris)
mvar_rec <- recipe(Sepal.Length + Petal.Width ~ ., data = iris) %>%
  step_dummy(Species)

test_that('lm execution', {
  skip_on_cran()

  # passes interactively but not on R CMD check
  # expect_error(
  #   res <- fit(
  #     iris_basic,
  #     iris_form,
  #     data = iris,
  #     control = ctrl,
  #     engine = "lm"
  #   ),
  #   regexp = NA
  # )
  expect_error(
    res <- fit(
      iris_basic,
      x = iris[, num_pred],
      y = iris$Sepal.Length,
      engine = "lm",
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- fit(
      iris_basic,
      recipe = iris_rec,
      data = iris,
      engine = "lm",
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- fit(
      iris_basic,
      iris_bad_form,
      data = iris,
      engine = "lm",
      control = ctrl
    )
  )

  lm_form_catch <- fit(
    iris_basic,
    iris_bad_form,
    data = iris,
    engine = "lm",
    control = caught_ctrl
  )
  expect_true(inherits(lm_form_catch, "try-error"))

  ## multivariate y

  expect_error(
    res <- fit(
      iris_basic,
      cbind(Sepal.Width, Petal.Width) ~ .,
      data = iris,
      control = ctrl,
      engine = "lm"
    ),
    regexp = NA
  )

  # TODO: embeds data in call
  expect_error(
    res <- fit(
      iris_basic,
      recipe = mvar_rec,
      data = iris,
      engine = "lm",
      control = ctrl
    ),
    regexp = NA
  )

})

test_that('glmnet execution', {
  skip_on_cran()

  # passes interactively but not on R CMD check
  # expect_error(
  #   fit(
  #     iris_basic,
  #     iris_form,
  #     data = iris,
  #     engine = "glmnet",
  #     control = ctrl
  #   ),
  #   regexp = NA
  # )

  expect_error(
    fit(
      iris_basic,
      engine = "glmnet",
      control = ctrl,
      x = iris[, num_pred],
      y = iris$Sepal.Length
    ),
    regexp = NA
  )

  expect_error(
    fit(
      iris_basic,
      recipe = iris_rec,
      data = iris,
      engine = "glmnet",
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    fit(
      iris_basic,
      iris_bad_form,
      data = iris,
      engine = "glm",
      control = ctrl
    )
  )

  glmnet_xy_catch <- fit(
    iris_basic,
    x = iris[, num_pred],
    y = factor(iris$Sepal.Length),
    engine = "glmnet",
    control = caught_ctrl
  )
  expect_true(inherits(glmnet_xy_catch, "try-error"))

})


test_that('stan_glm execution', {
  skip_on_cran()
  iris_basic_stan <- linear_reg(others = list(seed = 1333))

  # passes interactively but not on R CMD check
  # expect_error(
  #   res <- fit(
  #     iris_basic_stan,
  #     iris_form,
  #     data = iris,
  #     engine = "stan",
  #     control = ctrl
  #   ),
  #   regexp = NA
  # )

  expect_error(
    res <- fit(
      iris_basic,
      x = iris[, num_pred],
      y = iris$Sepal.Length,
      engine = "stan",
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    res <- fit(
      iris_basic,
      recipe = iris_rec,
      data = iris,
      engine = "stan",
      control = ctrl
    ),
    regexp = NA
  )

  expect_silent(
    res <- fit(
      iris_basic,
      recipe = iris_rec,
      data = iris,
      engine = "stan",
      control = quiet_ctrl
    )
  )

  expect_error(
    res <- fit(
      iris_basic,
      iris_bad_form,
      data = iris,
      engine = "stan",
      control = ctrl
    )
  )

  stan_form_catch <- fit(
    iris_basic,
    iris_bad_form,
    data = iris,
    engine = "stan",
    control = caught_ctrl
  )
  expect_true(inherits(stan_form_catch, "try-error"))

  stan_xy_catch <- fit(
    iris_basic,
    engine = "stan",
    control = caught_ctrl,
    x = iris[, num_pred],
    y = factor(iris$Sepal.Length)
  )
  expect_true(inherits(stan_xy_catch, "try-error"))

  stan_rec_catch <- fit(
    iris_basic,
    recipe = bad_rec,
    data = iris,
    engine = "stan",
    control = caught_ctrl
  )
  expect_true(inherits(stan_rec_catch, "try-error"))
})

