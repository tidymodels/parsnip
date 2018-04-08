library(testthat)
library(parsnip)
library(recipes)
library(rlang)

#TODO add spark test cases (in another file that is ignored on build?)

test_that('primary arguments', {
  basic <- logistic_reg()
  basic_glm <- translate(basic, engine = "glm")
  basic_glmnet <- translate(basic, engine = "glmnet")
  basic_stan <- translate(basic, engine = "stan")
  basic_spark <- translate(basic, engine = "spark")
  expect_equal(basic_glm$method$fit_args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 family = quote(binomial)
               )
  )
  expect_equal(basic_glmnet$method$fit_args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 family = "binomial"
               )
  )
  expect_equal(basic_stan$method$fit_args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 family = quote(binomial)
               )
  )
  expect_equal(basic_spark$method$fit_args,
               list(
                 x = quote(missing_arg()),
                 formula = quote(missing_arg()),
                 weight_col = quote(missing_arg()),
                 family = "binomial"
               )
  )

  mixture <- logistic_reg(mixture = 0.128)
  mixture_glmnet <- translate(mixture, engine = "glmnet")
  mixture_spark <- translate(mixture, engine = "spark")
  expect_equal(mixture_glmnet$method$fit_args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 alpha = 0.128,
                 family = "binomial"
               )
  )
  expect_equal(mixture_spark$method$fit_args,
               list(
                 x = quote(missing_arg()),
                 formula = quote(missing_arg()),
                 weight_col = quote(missing_arg()),
                 elastic_net_param = 0.128,
                 family = "binomial"
               )
  )

  regularization <- logistic_reg(regularization = 1)
  regularization_glmnet <- translate(regularization, engine = "glmnet")
  regularization_spark <- translate(regularization, engine = "spark")
  expect_equal(regularization_glmnet$method$fit_args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 lambda = 1,
                 family = "binomial"
               )
  )
  expect_equal(regularization_spark$method$fit_args,
               list(
                 x = quote(missing_arg()),
                 formula = quote(missing_arg()),
                 weight_col = quote(missing_arg()),
                 reg_param = 1,
                 family = "binomial"
               )
  )

  mixture_v <- logistic_reg(mixture = varying())
  mixture_v_glmnet <- translate(mixture_v, engine = "glmnet")
  mixture_v_spark <- translate(mixture_v, engine = "spark")
  expect_equal(mixture_v_glmnet$method$fit_args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 alpha = varying(),
                 family = "binomial"
               )
  )
  expect_equal(mixture_v_spark$method$fit_args,
               list(
                 x = quote(missing_arg()),
                 formula = quote(missing_arg()),
                 weight_col = quote(missing_arg()),
                 elastic_net_param = varying(),
                 family = "binomial"
               )
  )

})

test_that('engine arguments', {
  glm_fam <- logistic_reg(others = list(family = expr(binomial(link = "probit"))))
  expect_equal(translate(glm_fam, engine = "glm")$method$fit_args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 family = quote(binomial(link = "probit"))
               )
  )

  glmnet_nlam <- logistic_reg(others = list(nlambda = 10))
  expect_equal(translate(glmnet_nlam, engine = "glmnet")$method$fit_args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 nlambda = 10,
                 family = "binomial"
               )
  )

  stan_samp <- logistic_reg(others = list(chains = 1, iter = 5))
  expect_equal(translate(stan_samp, engine = "stan")$method$fit_args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 chains = 1,
                 iter = 5,
                 family = quote(binomial)
               )
  )

  spark_iter <- logistic_reg(others = list(max_iter = 20))
  expect_equal(translate(spark_iter, engine = "spark")$method$fit_args,
               list(
                 x = quote(missing_arg()),
                 formula = quote(missing_arg()),
                 weight_col = quote(missing_arg()),
                 max_iter = 20,
                 family = "binomial"
               )
  )

})


test_that('updating', {
  expr1     <- logistic_reg(             others = list(family = expr(binomial(link = "probit"))))
  expr1_exp <- logistic_reg(mixture = 0, others = list(family = expr(binomial(link = "probit"))))

  expr2     <- logistic_reg(mixture = varying())
  expr2_exp <- logistic_reg(mixture = varying(), others = list(nlambda = 10))

  expr3     <- logistic_reg(mixture = 0, regularization = varying())
  expr3_exp <- logistic_reg(mixture = 1)

  expr4     <- logistic_reg(mixture = 0, others = list(nlambda = 10))
  expr4_exp <- logistic_reg(mixture = 0, others = list(nlambda = 10, pmax = 2))

  expr5     <- logistic_reg(mixture = 1, others = list(nlambda = 10))
  expr5_exp <- logistic_reg(mixture = 1, others = list(nlambda = 10, pmax = 2))

  expect_equal(update(expr1, mixture = 0), expr1_exp)
  expect_equal(update(expr2, others = list(nlambda = 10)), expr2_exp)
  expect_equal(update(expr3, mixture = 1, fresh = TRUE), expr3_exp)
  expect_equal(update(expr4, others = list(pmax = 2)), expr4_exp)
  expect_equal(update(expr5, others = list(nlambda = 10, pmax = 2)), expr5_exp)

})

test_that('bad input', {
  expect_error(logistic_reg(ase.weights = var))
  expect_error(logistic_reg(mode = "regression"))
  expect_error(logistic_reg(regularization = -1))
  expect_error(logistic_reg(mixture = -1))
  expect_error(translate(logistic_reg(), engine = "wat?"))
  expect_warning(translate(logistic_reg(), engine = NULL))
  expect_error(translate(logistic_reg(formula = y ~ x)))
  expect_warning(translate(logistic_reg(others = list(x = iris[,1:3], y = iris$Species)), engine = "glmnet"))
  expect_warning(translate(logistic_reg(others = list(formula = y ~ x)), engine = "glm"))
})

###################################################################

data("lending_club")
lending_club <- head(lending_club, 200)
lc_form <- as.formula(Class ~ log(funded_amnt) + int_rate)
num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")
lc_bad_form <- as.formula(funded_amnt ~ term)
lc_basic <- logistic_reg()
ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)
lc_rec <- recipe(Class ~ funded_amnt + annual_inc + num_il_tl,
                 data = lending_club)
bad_rec <-
  recipe(total_bal_il ~ funded_amnt + annual_inc + num_il_tl,
         data = lending_club)


test_that('glm execution', {
  skip_on_cran()

  # passes interactively but not on R CMD check
  # expect_error(
  #   res <- fit(
  #     lc_basic,
  #     lc_form,
  #     data = lending_club,
  #     control = ctrl,
  #     engine = "glm"
  #   ),
  #   regexp = NA
  # )
  expect_error(
    res <- fit(
      lc_basic,
      x = lending_club[, num_pred],
      y = lending_club$Class,
      engine = "glm",
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- fit(
      lc_basic,
      recipe = lc_rec,
      data = lending_club,
      engine = "glm",
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- fit(
      lc_basic,
      lc_bad_form,
      data = lending_club,
      engine = "glm",
      control = ctrl
    )
  )

  glm_form_catch <- fit(
    lc_basic,
    lc_bad_form,
    data = lending_club,
    engine = "glm",
    control = caught_ctrl
  )
  expect_true(inherits(glm_form_catch, "try-error"))

  glm_xy_catch <- fit(
    lc_basic,
    engine = "glm",
    control = caught_ctrl,
    x = lending_club[, num_pred],
    y = lending_club$total_bal_il
  )
  expect_true(inherits(glm_xy_catch, "try-error"))

  glm_rec_catch <- fit(
    lc_basic,
    recipe = bad_rec,
    data = lending_club,
    engine = "glm",
    control = caught_ctrl
  )
  expect_true(inherits(glm_rec_catch, "try-error"))
})

test_that('glmnet execution', {
  skip_on_cran()

  # passes interactively but not on R CMD check
  # expect_error(
  #   fit(
  #     lc_basic,
  #     lc_form,
  #     data = lending_club,
  #     engine = "glmnet",
  #     control = ctrl
  #   ),
  #   regexp = NA
  # )

  expect_error(
    fit(
      lc_basic,
      engine = "glmnet",
      control = ctrl,
      x = lending_club[, num_pred],
      y = lending_club$Class
    ),
    regexp = NA
  )

  # TODO: fails because the recipe tries to convert a data frame containing a
  # factor to a matrix (and trips an error checker). This is supposed to work
  # well with multivariate data when the model interface is a matrix but it
  # shouldn't automatically do that for a single column non-numeric data set.
  # One more coded exception
  # expect_error(
  #   fit(
  #     lc_basic,
  #     recipe = lc_rec,
  #     data = lending_club,
  #     engine = "glmnet",
  #     control = ctrl
  #   ),
  #   regexp = NA
  # )

  # passes interactively but not on R CMD check
  # expect_error(
  #   fit(
  #     lc_basic,
  #     lc_bad_form,
  #     data = lending_club,
  #     engine = "glm",
  #     control = ctrl
  #   )
  # )

  # passes interactively but not on R CMD check
  # glmnet_form_catch <- fit(
  #   lc_basic,
  #   lc_bad_form,
  #   data = lending_club,
  #   engine = "glmnet",
  #   control = caught_ctrl
  # )
  # expect_true(inherits(glmnet_form_catch, "try-error"))

  glmnet_xy_catch <- fit(
    lc_basic,
    x = lending_club[, num_pred],
    y = lending_club$total_bal_il,
    engine = "glmnet",
    control = caught_ctrl
  )
  expect_true(inherits(glmnet_xy_catch, "try-error"))

  glmnet_rec_catch <- fit(
    lc_basic,
    recipe = bad_rec,
    data = lending_club,
    engine = "glmnet",
    control = caught_ctrl
  )
  expect_true(inherits(glmnet_rec_catch, "try-error"))
})


test_that('stan_glm execution', {
  skip_on_cran()
  lc_basic_stan <- logistic_reg(others = list(seed = 1333))

  # passes interactively but not on R CMD check
  # expect_error(
  #   res <- fit(
  #     lc_basic_stan,
  #     lc_form,
  #     data = lending_club,
  #     engine = "stan",
  #     control = ctrl
  #   ),
  #   regexp = NA
  # )

  expect_error(
    res <- fit(
      lc_basic,
      x = lending_club[, num_pred],
      y = lending_club$Class,
      engine = "stan",
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    res <- fit(
      lc_basic,
      recipe = lc_rec,
      data = lending_club,
      engine = "stan",
      control = ctrl
    ),
    regexp = NA
  )

  expect_silent(
    res <- fit(
      lc_basic,
      recipe = lc_rec,
      data = lending_club,
      engine = "stan",
      control = quiet_ctrl
    )
  )

  expect_error(
    res <- fit(
      lc_basic,
      lc_bad_form,
      data = lending_club,
      engine = "stan",
      control = ctrl
    )
  )

  stan_form_catch <- fit(
    lc_basic,
    lc_bad_form,
    data = lending_club,
    engine = "stan",
    control = caught_ctrl
  )
  expect_true(inherits(stan_form_catch, "try-error"))

  stan_xy_catch <- fit(
    lc_basic,
    engine = "stan",
    control = caught_ctrl,
    x = lending_club[, num_pred],
    y = lending_club$total_bal_il
  )
  expect_true(inherits(stan_xy_catch, "try-error"))

  stan_rec_catch <- fit(
    lc_basic,
    recipe = bad_rec,
    data = lending_club,
    engine = "stan",
    control = caught_ctrl
  )
  expect_true(inherits(stan_rec_catch, "try-error"))
})

