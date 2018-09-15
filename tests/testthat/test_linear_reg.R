library(testthat)
context("linear regression")
library(parsnip)
library(rlang)

test_that('primary arguments', {
  basic <- linear_reg()
  basic_lm <- translate(basic, engine = "lm")
  basic_glmnet <- translate(basic, engine = "glmnet")
  basic_stan <- translate(basic, engine = "stan")
  basic_spark <- translate(basic, engine = "spark")
  expect_equal(basic_lm$method$fit$args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg())
               )
  )
  expect_equal(basic_glmnet$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 family = "gaussian"
               )
  )
  expect_equal(basic_stan$method$fit$args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 family = "gaussian"
               )
  )
  expect_equal(basic_spark$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 formula = quote(missing_arg()),
                 weight_col = quote(missing_arg())
               )
  )

  mixture <- linear_reg(mixture = 0.128)
  mixture_glmnet <- translate(mixture, engine = "glmnet")
  mixture_spark <- translate(mixture, engine = "spark")
  expect_equal(mixture_glmnet$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 alpha = 0.128,
                 family = "gaussian"
               )
  )
  expect_equal(mixture_spark$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 formula = quote(missing_arg()),
                 weight_col = quote(missing_arg()),
                 elastic_net_param = 0.128
               )
  )

  penalty <- linear_reg(penalty = 1)
  penalty_glmnet <- translate(penalty, engine = "glmnet")
  penalty_spark <- translate(penalty, engine = "spark")
  expect_equal(penalty_glmnet$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 lambda = 1,
                 family = "gaussian"
               )
  )
  expect_equal(penalty_spark$method$fit$args,
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
  expect_equal(mixture_v_glmnet$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 alpha = varying(),
                 family = "gaussian"
               )
  )
  expect_equal(mixture_v_spark$method$fit$args,
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
  expect_equal(translate(lm_fam, engine = "lm")$method$fit$args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 model = FALSE
               )
  )

  glmnet_nlam <- linear_reg(others = list(nlambda = 10))
  expect_equal(translate(glmnet_nlam, engine = "glmnet")$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 nlambda = 10,
                 family = "gaussian"
               )
  )

  stan_samp <- linear_reg(others = list(chains = 1, iter = 5))
  expect_equal(translate(stan_samp, engine = "stan")$method$fit$args,
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
  expect_equal(translate(spark_iter, engine = "spark")$method$fit$args,
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

  expr3     <- linear_reg(mixture = 0, penalty = varying())
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
  expect_error(linear_reg(penalty = -1))
  expect_error(linear_reg(mixture = -1))
  expect_error(translate(linear_reg(), engine = "wat?"))
  expect_warning(translate(linear_reg(), engine = NULL))
  expect_error(translate(linear_reg(formula = y ~ x)))
  expect_warning(translate(linear_reg(others = list(x = iris[,1:3], y = iris$Species)), engine = "glmnet"))
  expect_warning(translate(linear_reg(others = list(formula = y ~ x)), engine = "lm"))
})

###################################################################

num_pred <- c("Sepal.Width", "Petal.Width", "Petal.Length")
iris_bad_form <- as.formula(Species ~ term)
iris_basic <- linear_reg()
ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

test_that('lm execution', {


  # passes interactively but not on R CMD check
  # expect_error(
  #   res <- fit(
  #     iris_basic,
  #     Sepal.Length ~ log(Sepal.Width) + Species,
  #     data = iris,
  #     control = ctrl,
  #     engine = "lm"
  #   ),
  #   regexp = NA
  # )
  expect_error(
    res <- fit_xy(
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
      iris_bad_form,
      data = iris,
      engine = "lm",
      control = ctrl
    )
  )

  # passes interactively but not on R CMD check
  # lm_form_catch <- fit(
  #   iris_basic,
  #   iris_bad_form,
  #   data = iris,
  #   engine = "lm",
  #   control = caught_ctrl
  # )
  # expect_true(inherits(lm_form_catch$fit, "try-error"))

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

})

test_that('lm prediction', {
  uni_lm <- lm(Sepal.Length ~ Sepal.Width + Petal.Width + Petal.Length, data = iris)
  uni_pred <- unname(predict(uni_lm, newdata = iris[1:5, ]))
  inl_lm <- lm(Sepal.Length ~ log(Sepal.Width) + Species, data = iris)
  inl_pred <- unname(predict(inl_lm, newdata = iris[1:5, ]))
  mv_lm <- lm(cbind(Sepal.Width, Petal.Width) ~ ., data = iris)
  mv_pred <- as.data.frame(predict(mv_lm, newdata = iris[1:5, ]))

  res_xy <- fit_xy(
    iris_basic,
    x = iris[, num_pred],
    y = iris$Sepal.Length,
    engine = "lm",
    control = ctrl
  )

  expect_equal(uni_pred, predict_num(res_xy, iris[1:5, num_pred]))

  res_form <- fit(
    iris_basic,
    Sepal.Length ~ log(Sepal.Width) + Species,
    data = iris,
    engine = "lm",
    control = ctrl
  )
  expect_equal(inl_pred, predict_num(res_form, iris[1:5, ]))

  res_mv <- fit(
    iris_basic,
    cbind(Sepal.Width, Petal.Width) ~ .,
    data = iris,
    control = ctrl,
    engine = "lm"
  )
  expect_equal(mv_pred, predict_num(res_mv, iris[1:5,]))
})



test_that('lm intervals', {
  stats_lm <- lm(Sepal.Length ~ Sepal.Width + Petal.Width + Petal.Length,
                 data = iris)
  confidence_lm <- predict(stats_lm, newdata = iris[1:5, ], 
                           level = 0.93, interval = "confidence")
  prediction_lm <- predict(stats_lm, newdata = iris[1:5, ], 
                           level = 0.93, interval = "prediction")
  
  res_xy <- fit_xy(
    linear_reg(),
    x = iris[, num_pred],
    y = iris$Sepal.Length,
    engine = "lm",
    control = ctrl
  )
  
  confidence_parsnip <-
    predict(res_xy,
            new_data = iris[1:5,],
            type = "conf_int",
            level = 0.93)
  
  expect_equivalent(confidence_parsnip$.pred_lower, confidence_lm[, "lwr"])
  expect_equivalent(confidence_parsnip$.pred_upper, confidence_lm[, "upr"])
  
  prediction_parsnip <-
    predict(res_xy,
            new_data = iris[1:5,],
            type = "pred_int",
            level = 0.93)

  expect_equivalent(prediction_parsnip$.pred_lower, prediction_lm[, "lwr"])
  expect_equivalent(prediction_parsnip$.pred_upper, prediction_lm[, "upr"])
})

