library(testthat)
library(parsnip)
library(rlang)
library(tibble)

# ------------------------------------------------------------------------------

context("linear regression")
source(test_path("helpers.R"))
source(test_path("helper-objects.R"))
hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

test_that('primary arguments', {
  basic <- linear_reg()
  basic_lm <- translate(basic %>% set_engine("lm"))
  basic_glm <- translate(basic %>% set_engine("glm"))
  expect_error(
    basic_glmnet <- translate(basic %>% set_engine("glmnet")),
    "For the glmnet engine, `penalty` must be a single"
  )
  basic_stan <- translate(basic %>% set_engine("stan"))
  basic_spark <- translate(basic %>% set_engine("spark"))
  expect_equal(basic_lm$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg())
               )
  )
  expect_equal(basic_glm$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 family = expr(stats::gaussian)
               )
  )
  expect_equal(basic_stan$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 family = expr(stats::gaussian),
                 refresh = 0
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
  expect_error(
    mixture_glmnet <- translate(mixture %>% set_engine("glmnet")),
    "For the glmnet engine, `penalty` must be a single"
  )
  mixture_spark <- translate(mixture %>% set_engine("spark"))
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
  expect_error(
    mixture_v_glmnet <- translate(mixture_v %>% set_engine("glmnet")),
    "For the glmnet engine, `penalty` must be a single"
  )
  mixture_v_spark <- translate(mixture_v %>% set_engine("spark"))
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

  glm_log <- linear_reg() %>% set_engine("glm", family = "quasipoisson")
  expect_equal(translate(glm_log)$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 family = new_empty_quosure("quasipoisson")
               )
  )

  glmnet_nlam <- linear_reg(penalty = 0.1) %>% set_engine("glmnet", nlambda = 10)
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
                 family = expr(stats::gaussian),
                 refresh = 0
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

  # For issue #431
  with_path <-
    linear_reg(penalty = 1) %>%
    set_engine("glmnet", path_values = 4:2) %>%
    translate()
  expect_equal(
    names(with_path$method$fit$args),
    c("x", "y", "weights", "lambda", "family")
  )
  expect_equal(
    rlang::eval_tidy(with_path$method$fit$args$lambda),
    4:2
  )
})


test_that('updating', {
  expr1     <- linear_reg() %>% set_engine("lm", model = FALSE)
  expr1_exp <- linear_reg(mixture = 0) %>% set_engine("lm", model = FALSE)

  expr2     <- linear_reg() %>% set_engine("glmnet", nlambda = varying())
  expr2_exp <- linear_reg() %>% set_engine("glmnet", nlambda = 10)

  expr3     <- linear_reg(mixture = 0, penalty = varying()) %>% set_engine("glmnet", nlambda = varying())
  expr3_exp <- linear_reg(mixture = 0, penalty = varying()) %>% set_engine("glmnet", nlambda = 10)
  expr3_fre <- linear_reg(mixture = 1) %>% set_engine("glmnet", nlambda = 10)

  expr4     <- linear_reg(mixture = 0) %>% set_engine("glmnet", nlambda = 10)
  expr4_exp <- linear_reg(mixture = 0) %>% set_engine("glmnet", nlambda = 10, pmax = 2)

  expr5     <- linear_reg(mixture = 1) %>% set_engine("glmnet", nlambda = 10)
  expr5_exp <- linear_reg(mixture = 1) %>% set_engine("glmnet", nlambda = 10, pmax = 2)

  expr6     <- linear_reg() %>% set_engine("glm", link = "identity")
  expr6_exp <- linear_reg() %>% set_engine("glm", link = "log")

  expect_equal(update(expr1, mixture = 0), expr1_exp)
  expect_equal(update(expr2, nlambda = 10), expr2_exp)
  expect_equal(update(expr3, mixture = 1, fresh = TRUE, nlambda = 10), expr3_fre)
  expect_equal(update(expr3, nlambda = 10), expr3_exp)
  expect_equal(update(expr6, link = "log"), expr6_exp)

  param_tibb <- tibble::tibble(mixture = 1/3, penalty = 1)
  param_list <- as.list(param_tibb)

  expr4_updated <- update(expr4, param_tibb)
  expect_equal(expr4_updated$args$mixture, 1/3)
  expect_equal(expr4_updated$args$penalty, 1)
  expect_equal(expr4_updated$eng_args$nlambda, rlang::quo(10))

  expr4_updated_lst <- update(expr4, param_list)
  expect_equal(expr4_updated_lst$args$mixture, 1/3)
  expect_equal(expr4_updated_lst$args$penalty, 1)
  expect_equal(expr4_updated_lst$eng_args$nlambda, rlang::quo(10))

})

test_that('bad input', {
  expect_error(linear_reg(mode = "classification"))
  # expect_error(linear_reg(penalty = -1))
  # expect_error(linear_reg(mixture = -1))
  expect_error(translate(linear_reg(), engine = "wat?"))
  expect_error(translate(linear_reg(), engine = NULL))
  expect_error(translate(linear_reg(formula = y ~ x)))
  expect_error(translate(linear_reg(x = hpc[,1:3], y = hpc$class) %>% set_engine("glmnet")))
  expect_error(translate(linear_reg(formula = y ~ x)  %>% set_engine("lm")))
})

# ------------------------------------------------------------------------------

num_pred <- c("input_fields", "num_pending", "iterations")
hpc_bad_form <- as.formula(class ~ term)
hpc_basic <- linear_reg() %>% set_engine("lm")

# ------------------------------------------------------------------------------

test_that('lm execution', {

  expect_error(
    res <- fit(
      hpc_basic,
      input_fields ~ log(compounds) + class,
      data = hpc,
      control = ctrl
    ),
    regexp = NA
  )
  expect_output(print(res), "parsnip model object")

  expect_error(
    res <- fit_xy(
      hpc_basic,
      x = hpc[, num_pred],
      y = hpc$input_fields,
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    res <- fit_xy(
      hpc_basic,
      x = hpc[, num_pred],
      y = hpc$class,
      control = ctrl
    ),
    regexp = "For a regression model"
  )

  expect_error(
    res <- fit(
      hpc_basic,
      hpc_bad_form,
      data = hpc,
      control = ctrl
    )
  )

  expect_error(
    lm_form_catch <- fit(
      hpc_basic,
      hpc_bad_form,
      data = hpc,
      control = caught_ctrl
    ),
    regexp = "For a regression model"
  )

  ## multivariate y

  expect_error(
    res <- fit(
      hpc_basic,
      cbind(compounds, iterations) ~ .,
      data = hpc,
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    res <- fit_xy(
      hpc_basic,
      x = hpc[, 1:2],
      y = hpc[3:4],
      control = ctrl
    ),
    regexp = NA
  )
})

test_that('glm execution', {

  hpc_glm <- linear_reg() %>% set_engine("glm")

  expect_error(
    res <- fit(
      hpc_glm,
      input_fields ~ log(compounds) + class,
      data = hpc,
      control = ctrl
    ),
    regexp = NA
  )
  expect_output(print(res), "parsnip model object")

  expect_error(
    res <- fit_xy(
      hpc_glm,
      x = hpc[, num_pred],
      y = hpc$input_fields,
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    res <- fit_xy(
      hpc_glm,
      x = hpc[, num_pred],
      y = hpc$class,
      control = ctrl
    ),
    regexp = "For a regression model"
  )

  expect_error(
    res <- fit(
      hpc_glm,
      hpc_bad_form,
      data = hpc,
      control = ctrl
    )
  )

  expect_error(
    lm_form_catch <- fit(
      hpc_glm,
      hpc_bad_form,
      data = hpc,
      control = caught_ctrl
    ),
    regexp = "For a regression model"
  )

})


test_that('lm prediction', {
  uni_lm <- lm(compounds ~ input_fields + num_pending + iterations, data = hpc)
  uni_pred <- unname(predict(uni_lm, newdata = hpc[1:5, ]))
  inl_lm <- lm(compounds ~ log(input_fields) + class, data = hpc)
  inl_pred <- unname(predict(inl_lm, newdata = hpc[1:5, ]))
  mv_lm <- lm(cbind(input_fields, num_pending) ~ ., data = hpc)
  mv_pred <- as_tibble(predict(mv_lm, newdata = hpc[1:5, ]))
  names(mv_pred) <- c(".pred_input_fields", ".pred_num_pending")

  res_xy <- fit_xy(
    hpc_basic,
    x = hpc[, num_pred],
    y = hpc$compounds,
    control = ctrl
  )

  expect_equal(uni_pred, predict(res_xy, hpc[1:5, num_pred])$.pred)

  res_form <- fit(
    hpc_basic,
    compounds ~ log(input_fields) + class,
    data = hpc,
    control = ctrl
  )

  expect_equal(inl_pred, predict(res_form, hpc[1:5, ])$.pred)

  res_mv <- fit(
    hpc_basic,
    cbind(input_fields, num_pending) ~ .,
    data = hpc,
    control = ctrl
  )

  expect_equal(mv_pred, predict(res_mv, hpc[1:5,]))
})

test_that('glm prediction', {

  hpc_glm <- linear_reg() %>% set_engine("glm")

  uni_lm <- glm(compounds ~ input_fields + num_pending + iterations, data = hpc)
  uni_pred <- unname(predict(uni_lm, newdata = hpc[1:5, ]))
  inl_lm <- glm(compounds ~ log(input_fields) + class, data = hpc)
  inl_pred <- unname(predict(inl_lm, newdata = hpc[1:5, ]))

  res_xy <- fit_xy(
    hpc_glm,
    x = hpc[, num_pred],
    y = hpc$compounds,
    control = ctrl
  )

  expect_equal(uni_pred, predict(res_xy, hpc[1:5, num_pred])$.pred)

  res_form <- fit(
    hpc_glm,
    compounds ~ log(input_fields) + class,
    data = hpc,
    control = ctrl
  )

  expect_equal(inl_pred, predict(res_form, hpc[1:5, ])$.pred)

})

test_that('lm intervals', {
  stats_lm <- lm(compounds ~ input_fields + iterations + num_pending,
                 data = hpc)
  confidence_lm <- predict(stats_lm, newdata = hpc[1:5, ],
                           level = 0.93, interval = "confidence")
  prediction_lm <- predict(stats_lm, newdata = hpc[1:5, ],
                           level = 0.93, interval = "prediction")

  res_xy <- fit_xy(
    linear_reg()  %>% set_engine("lm"),
    x = hpc[, num_pred],
    y = hpc$compounds,
    control = ctrl
  )

  confidence_parsnip <-
    predict(res_xy,
            new_data = hpc[1:5,],
            type = "conf_int",
            level = 0.93)

  expect_equivalent(confidence_parsnip$.pred_lower, confidence_lm[, "lwr"])
  expect_equivalent(confidence_parsnip$.pred_upper, confidence_lm[, "upr"])

  prediction_parsnip <-
    predict(res_xy,
            new_data = hpc[1:5,],
            type = "pred_int",
            level = 0.93)

  expect_equivalent(prediction_parsnip$.pred_lower, prediction_lm[, "lwr"])
  expect_equivalent(prediction_parsnip$.pred_upper, prediction_lm[, "upr"])
})

test_that('glm intervals', {
  stats_glm <- glm(compounds ~ input_fields + iterations + num_pending,
                   data = hpc)
  pred_glm <- predict(stats_glm, newdata = hpc[1:5, ], se.fit = TRUE)
  t_val <- qt(0.035, df = stats_glm$df.residual, lower.tail = FALSE)
  lower_glm <- pred_glm$fit - t_val * pred_glm$se.fit
  upper_glm <- pred_glm$fit + t_val * pred_glm$se.fit

  lower_glm <- stats_glm$family$linkinv(lower_glm)
  upper_glm <- stats_glm$family$linkinv(upper_glm)

  res_xy <- fit_xy(
    linear_reg()  %>% set_engine("glm"),
    x = hpc[, num_pred],
    y = hpc$compounds,
    control = ctrl
  )

  confidence_parsnip <-
    predict(res_xy,
            new_data = hpc[1:5,],
            type = "conf_int",
            level = 0.93)

  expect_equivalent(confidence_parsnip$.pred_lower, lower_glm)
  expect_equivalent(confidence_parsnip$.pred_upper, upper_glm)

})


test_that('newdata error trapping', {
  res_xy <- fit_xy(
    hpc_basic,
    x = hpc[, num_pred],
    y = hpc$input_fields,
    control = ctrl
  )
  expect_error(predict(res_xy, newdata = hpc[1:3, num_pred]), "Did you mean")
})

test_that('show engine', {
  res <- show_engines("linear_reg")
  expt <- get_from_env("linear_reg")
  expect_equal(res, expt)
  expect_error(show_engines("linear_re"), "No results found for model function")
})

