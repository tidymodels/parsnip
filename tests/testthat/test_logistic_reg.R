library(testthat)
library(parsnip)
library(rlang)
library(tibble)

# ------------------------------------------------------------------------------

context("logistic regression")
source("helpers.R")

# ------------------------------------------------------------------------------

test_that('primary arguments', {
  basic <- logistic_reg()
  basic_glm <- translate(basic %>% set_engine("glm"))
  basic_glmnet <- translate(basic %>% set_engine("glmnet"))
  basic_stan <- translate(basic %>% set_engine("stan"))
  basic_spark <- translate(basic %>% set_engine("spark"))
  expect_equal(basic_glm$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 family = expr(stats::binomial)
               )
  )
  expect_equal(basic_glmnet$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 family = "binomial"
               )
  )
  expect_equal(basic_stan$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 family = expr(stats::binomial)
               )
  )
  expect_equal(basic_spark$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 formula = expr(missing_arg()),
                 weight_col = expr(missing_arg()),
                 family = "binomial"
               )
  )

  mixture <- logistic_reg(mixture = 0.128)
  mixture_glmnet <- translate(mixture %>% set_engine("glmnet"))
  mixture_spark <- translate(mixture %>% set_engine("spark"))
  expect_equal(mixture_glmnet$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 alpha = new_empty_quosure(0.128),
                 family = "binomial"
               )
  )
  expect_equal(mixture_spark$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 formula = expr(missing_arg()),
                 weight_col = expr(missing_arg()),
                 elastic_net_param = new_empty_quosure(0.128),
                 family = "binomial"
               )
  )

  penalty <- logistic_reg(penalty = 1)
  penalty_glmnet <- translate(penalty %>% set_engine("glmnet"))
  penalty_spark <- translate(penalty %>% set_engine("spark"))
  expect_equal(penalty_glmnet$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 lambda = new_empty_quosure(1),
                 family = "binomial"
               )
  )
  expect_equal(penalty_spark$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 formula = expr(missing_arg()),
                 weight_col = expr(missing_arg()),
                 reg_param = new_empty_quosure(1),
                 family = "binomial"
               )
  )

  mixture_v <- logistic_reg(mixture = varying())
  mixture_v_glmnet <- translate(mixture_v %>% set_engine("glmnet"))
  mixture_v_spark <- translate(mixture_v %>% set_engine("spark"))
  expect_equal(mixture_v_glmnet$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 alpha = new_empty_quosure(varying()),
                 family = "binomial"
               )
  )
  expect_equal(mixture_v_spark$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 formula = expr(missing_arg()),
                 weight_col = expr(missing_arg()),
                 elastic_net_param = new_empty_quosure(varying()),
                 family = "binomial"
               )
  )

})

test_that('engine arguments', {
  glm_fam <- logistic_reg()
  expect_equal(
    translate(
      glm_fam %>%
        set_engine("glm", family = binomial(link = "probit")))$method$fit$args,
      list(
        formula = expr(missing_arg()),
        data = expr(missing_arg()),
        weights = expr(missing_arg()),
        family = new_empty_quosure(expr(binomial(link = "probit")))
      )
    )

  glmnet_nlam <- logistic_reg()
  expect_equal(
    translate(glmnet_nlam %>% set_engine("glmnet", nlambda = 10))$method$fit$args,
    list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      weights = expr(missing_arg()),
      nlambda = new_empty_quosure(10),
      family = "binomial"
    )
  )

  stan_samp <- logistic_reg()
  expect_equal(
    translate(stan_samp %>% set_engine("stan", chains = 1, iter = 5))$method$fit$args,
    list(
      formula = expr(missing_arg()),
      data = expr(missing_arg()),
      weights = expr(missing_arg()),
      chains = new_empty_quosure(1),
      iter = new_empty_quosure(5),
      family = expr(stats::binomial)
    )
  )

  spark_iter <- logistic_reg()
  expect_equal(
    translate(spark_iter %>% set_engine("spark", max_iter = 20))$method$fit$args,
    list(
      x = expr(missing_arg()),
      formula = expr(missing_arg()),
      weight_col = expr(missing_arg()),
      max_iter = new_empty_quosure(20),
      family = "binomial"
    )
  )

})


test_that('updating', {
  expr1     <- logistic_reg() %>%
    set_engine("glm", family = expr(binomial(link = "probit")))
  expr1_exp <- logistic_reg(mixture = 0) %>%
    set_engine("glm", family = expr(binomial(link = "probit")))

  expr2     <- logistic_reg(mixture = varying()) %>% set_engine("glmnet")
  expr2_exp <- logistic_reg(mixture = varying()) %>% set_engine("glmnet", nlambda = 10)

  expr3     <- logistic_reg(mixture = 0, penalty = varying())
  expr3_exp <- logistic_reg(mixture = 1)

  expr4     <- logistic_reg(mixture = 0) %>% set_engine("glmnet", nlambda = 10)
  expr4_exp <- logistic_reg(mixture = 0) %>% set_engine("glmnet", nlambda = 10, pmax = 2)

  expr5     <- logistic_reg(mixture = 1) %>% set_engine("glmnet", nlambda = 10)
  expr5_exp <- logistic_reg(mixture = 1) %>% set_engine("glmnet", nlambda = 10, pmax = 2)

  expect_equal(update(expr1, mixture = 0), expr1_exp)
  expect_equal(update(expr3, mixture = 1, fresh = TRUE), expr3_exp)

})

test_that('bad input', {
  expect_error(logistic_reg(mode = "regression"))
  expect_error(translate(logistic_reg(formula = y ~ x)))
  expect_error(translate(logistic_reg(x = iris[,1:3], y = iris$Species) %>% set_engine(engine = "glmnet")))
  expect_error(translate(logistic_reg(formula = y ~ x) %>% set_engine(engine = "glm")))
})

# ------------------------------------------------------------------------------

data("lending_club")
lending_club <- head(lending_club, 200)
lc_form <- as.formula(Class ~ log(funded_amnt) + int_rate)
num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")
lc_basic <- logistic_reg() %>% set_engine("glm")
ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

test_that('glm execution', {


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
    res <- fit_xy(
      lc_basic,
      x = lending_club[, num_pred],
      y = lending_club$Class,
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- fit(
      lc_basic,
      funded_amnt ~ term,
      data = lending_club,
      control = ctrl
    )
  )

  # passes interactively but not on R CMD check
  # glm_form_catch <- fit(
  #   lc_basic,
  #   funded_amnt ~ term,
  #   data = lending_club,
  #
  #   control = caught_ctrl
  # )
  # expect_true(inherits(glm_form_catch$fit, "try-error"))

  glm_xy_catch <- fit_xy(
    lc_basic,
    control = caught_ctrl,
    x = lending_club[, num_pred],
    y = lending_club$total_bal_il
  )
  expect_true(inherits(glm_xy_catch$fit, "try-error"))
})

test_that('glm prediction', {
  classes_xy <- fit_xy(
    lc_basic,
    x = lending_club[, num_pred],
    y = lending_club$Class,
    control = ctrl
  )

  xy_pred <- predict(classes_xy$fit, newdata = lending_club[1:7, num_pred], type = "response")
  xy_pred <- ifelse(xy_pred >= 0.5, "good", "bad")
  xy_pred <- factor(xy_pred, levels = levels(lending_club$Class))
  xy_pred <- unname(xy_pred)
  expect_equal(xy_pred, predict_class(classes_xy, lending_club[1:7, num_pred]))

})

test_that('glm probabilities', {
  classes_xy <- fit_xy(
    lc_basic,
    x = lending_club[, num_pred],
    y = lending_club$Class,
    control = ctrl
  )

  xy_pred <- predict(classes_xy$fit, newdata = lending_club[1:7, num_pred], type = "response")
  xy_pred <- tibble(bad = 1 - xy_pred, good = xy_pred)
  expect_equal(xy_pred, predict_classprob(classes_xy, lending_club[1:7, num_pred]))

  one_row <- predict_classprob(classes_xy, lending_club[1, num_pred])
  expect_equal(xy_pred[1,], one_row)

})



test_that('glm intervals', {
  stats_glm <- glm(Class ~ log(funded_amnt) + int_rate, data = lending_club,
                   family = binomial)
  pred_glm <- predict(stats_glm, newdata = lending_club[1:5, ], se.fit = TRUE)
  t_val <- qt(0.035, df = stats_glm$df.residual, lower.tail = FALSE)
  lower_glm <- pred_glm$fit - t_val * pred_glm$se.fit
  upper_glm <- pred_glm$fit + t_val * pred_glm$se.fit

  lower_glm <- stats_glm$family$linkinv(lower_glm)
  upper_glm <- stats_glm$family$linkinv(upper_glm)

  res <- fit(
    logistic_reg() %>% set_engine("glm"),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    control = ctrl
  )

  confidence_parsnip <-
    predict(res,
            new_data = lending_club[1:5,],
            type = "conf_int",
            level = 0.93,
            std_error = TRUE)

  expect_equivalent(confidence_parsnip$.pred_lower, lower_glm)
  expect_equivalent(confidence_parsnip$.pred_upper, upper_glm)
  expect_equivalent(confidence_parsnip$.std_error, pred_glm$se.fit)

})

