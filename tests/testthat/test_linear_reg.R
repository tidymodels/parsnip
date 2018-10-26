library(testthat)
library(parsnip)
library(rlang)

# ------------------------------------------------------------------------------

context("linear regression")
source("helpers.R")

# ------------------------------------------------------------------------------

test_that('primary arguments', {
  basic <- linear_reg()
  basic_lm <- translate(basic %>% set_engine("lm"))
  basic_glmnet <- translate(basic %>% set_engine("glmnet"))
  basic_stan <- translate(basic %>% set_engine("stan"))
  basic_spark <- translate(basic %>% set_engine("spark"))
  expect_equal(basic_lm$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg())
               )
  )
  expect_equal(basic_glmnet$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 family = "gaussian"
               )
  )
  expect_equal(basic_stan$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 family = expr(stats::gaussian)
               )
  )
  expect_equal(basic_spark$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 formula = expr(missing_arg()),
                 weight_col = expr(missing_arg())
               )
  )

  mixture <- linear_reg(mixture = 0.128)
  mixture_glmnet <- translate(mixture %>% set_engine("glmnet"))
  mixture_spark <- translate(mixture %>% set_engine("spark"))
  expect_equal(mixture_glmnet$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 alpha = new_empty_quosure(0.128),
                 family = "gaussian"
               )
  )
  expect_equal(mixture_spark$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 formula = expr(missing_arg()),
                 weight_col = expr(missing_arg()),
                 elastic_net_param = new_empty_quosure(0.128)
               )
  )

  penalty <- linear_reg(penalty = 1)
  penalty_glmnet <- translate(penalty %>% set_engine("glmnet"))
  penalty_spark <- translate(penalty %>% set_engine("spark"))
  expect_equal(penalty_glmnet$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 lambda = new_empty_quosure(1),
                 family = "gaussian"
               )
  )
  expect_equal(penalty_spark$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 formula = expr(missing_arg()),
                 weight_col = expr(missing_arg()),
                 reg_param = new_empty_quosure(1)
               )
  )

  mixture_v <- linear_reg(mixture = varying())
  mixture_v_glmnet <- translate(mixture_v %>% set_engine("glmnet"))
  mixture_v_spark <- translate(mixture_v %>% set_engine("spark"))
  expect_equal(mixture_v_glmnet$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 alpha = new_empty_quosure(varying()),
                 family = "gaussian"
               )
  )
  expect_equal(mixture_v_spark$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 formula = expr(missing_arg()),
                 weight_col = expr(missing_arg()),
                 elastic_net_param = new_empty_quosure(varying())
               )
  )

})

test_that('engine arguments', {
  lm_fam <- linear_reg() %>% set_engine("lm", model = FALSE)
  expect_equal(translate(lm_fam)$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 model = new_empty_quosure(FALSE)
               )
  )

  glmnet_nlam <- linear_reg() %>% set_engine("glmnet", nlambda = 10)
  expect_equal(translate(glmnet_nlam)$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 nlambda = new_empty_quosure(10),
                 family = "gaussian"
               )
  )

  stan_samp <- linear_reg() %>% set_engine("stan", chains = 1, iter = 5)
  expect_equal(translate(stan_samp)$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 chains = new_empty_quosure(1),
                 iter = new_empty_quosure(5),
                 family = expr(stats::gaussian)
               )
  )

  spark_iter <- linear_reg() %>% set_engine("spark", max_iter = 20)
  expect_equal(translate(spark_iter)$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 formula = expr(missing_arg()),
                 weight_col = expr(missing_arg()),
                 max_iter = new_empty_quosure(20)
               )
  )

})


test_that('updating', {
  expr1     <- linear_reg() %>% set_engine("lm", model = FALSE)
  expr1_exp <- linear_reg(mixture = 0) %>% set_engine("lm", model = FALSE)

  expr2     <- linear_reg(mixture = varying()) %>% set_engine("glmnet")
  expr2_exp <- linear_reg(mixture = varying()) %>% set_engine("glmnet", nlambda = 10)

  expr3     <- linear_reg(mixture = 0, penalty = varying()) %>% set_engine("glmnet")
  expr3_exp <- linear_reg(mixture = 1) %>% set_engine("glmnet")

  expr4     <- linear_reg(mixture = 0) %>% set_engine("glmnet", nlambda = 10)
  expr4_exp <- linear_reg(mixture = 0) %>% set_engine("glmnet", nlambda = 10, pmax = 2)

  expr5     <- linear_reg(mixture = 1) %>% set_engine("glmnet", nlambda = 10)
  expr5_exp <- linear_reg(mixture = 1) %>% set_engine("glmnet", nlambda = 10, pmax = 2)

  expect_equal(update(expr1, mixture = 0), expr1_exp)
  expect_equal(update(expr3, mixture = 1, fresh = TRUE), expr3_exp)

})

test_that('bad input', {
  expect_error(linear_reg(mode = "classification"))
  # expect_error(linear_reg(penalty = -1))
  # expect_error(linear_reg(mixture = -1))
  expect_error(translate(linear_reg(), engine = "wat?"))
  expect_error(translate(linear_reg(), engine = NULL))
  expect_error(translate(linear_reg(formula = y ~ x)))
  expect_error(translate(linear_reg(x = iris[,1:3], y = iris$Species) %>% set_engine("glmnet")))
  expect_error(translate(linear_reg(formula = y ~ x)  %>% set_engine("lm")))
})

# ------------------------------------------------------------------------------

num_pred <- c("Sepal.Width", "Petal.Width", "Petal.Length")
iris_bad_form <- as.formula(Species ~ term)
iris_basic <- linear_reg() %>% set_engine("lm")

ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

# ------------------------------------------------------------------------------

test_that('lm execution', {

  expect_error(
    res <- fit(
      iris_basic,
      Sepal.Length ~ log(Sepal.Width) + Species,
      data = iris,
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- fit_xy(
      iris_basic,
      x = iris[, num_pred],
      y = iris$Sepal.Length,
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    res <- fit(
      iris_basic,
      iris_bad_form,
      data = iris,
      control = ctrl
    )
  )

  lm_form_catch <- fit(
    iris_basic,
    iris_bad_form,
    data = iris,
    control = caught_ctrl
  )
  expect_true(inherits(lm_form_catch$fit, "try-error"))

  ## multivariate y

  expect_error(
    res <- fit(
      iris_basic,
      cbind(Sepal.Width, Petal.Width) ~ .,
      data = iris,
      control = ctrl
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
    control = ctrl
  )

  expect_equal(uni_pred, predict_numeric(res_xy, iris[1:5, num_pred]))

  res_form <- fit(
    iris_basic,
    Sepal.Length ~ log(Sepal.Width) + Species,
    data = iris,
    control = ctrl
  )
  expect_equal(inl_pred, predict_numeric(res_form, iris[1:5, ]))

  res_mv <- fit(
    iris_basic,
    cbind(Sepal.Width, Petal.Width) ~ .,
    data = iris,
    control = ctrl
  )
  expect_equal(mv_pred, predict_numeric(res_mv, iris[1:5,]))
})

test_that('lm intervals', {
  stats_lm <- lm(Sepal.Length ~ Sepal.Width + Petal.Width + Petal.Length,
                 data = iris)
  confidence_lm <- predict(stats_lm, newdata = iris[1:5, ],
                           level = 0.93, interval = "confidence")
  prediction_lm <- predict(stats_lm, newdata = iris[1:5, ],
                           level = 0.93, interval = "prediction")

  res_xy <- fit_xy(
    linear_reg()  %>% set_engine("lm"),
    x = iris[, num_pred],
    y = iris$Sepal.Length,
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


test_that('newdata error trapping', {
  res_xy <- fit_xy(
    iris_basic,
    x = iris[, num_pred],
    y = iris$Sepal.Length,
    control = ctrl
  )
  expect_error(predict(res_xy, newdata = iris[1:3, num_pred]), "Did you mean")
})

