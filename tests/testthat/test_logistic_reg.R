library(testthat)
context("logistic regression")
library(parsnip)
library(rlang)

test_that('primary arguments', {
  basic <- logistic_reg()
  basic_glm <- translate(basic, engine = "glm")
  basic_glmnet <- translate(basic, engine = "glmnet")
  basic_stan <- translate(basic, engine = "stan")
  basic_spark <- translate(basic, engine = "spark")
  expect_equal(basic_glm$method$fit$args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 family = quote(binomial)
               )
  )
  expect_equal(basic_glmnet$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 family = "binomial"
               )
  )
  expect_equal(basic_stan$method$fit$args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 family = quote(binomial)
               )
  )
  expect_equal(basic_spark$method$fit$args,
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
  expect_equal(mixture_glmnet$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 alpha = 0.128,
                 family = "binomial"
               )
  )
  expect_equal(mixture_spark$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 formula = quote(missing_arg()),
                 weight_col = quote(missing_arg()),
                 elastic_net_param = 0.128,
                 family = "binomial"
               )
  )

  penalty <- logistic_reg(penalty = 1)
  penalty_glmnet <- translate(penalty, engine = "glmnet")
  penalty_spark <- translate(penalty, engine = "spark")
  expect_equal(penalty_glmnet$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 lambda = 1,
                 family = "binomial"
               )
  )
  expect_equal(penalty_spark$method$fit$args,
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
  expect_equal(mixture_v_glmnet$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 alpha = varying(),
                 family = "binomial"
               )
  )
  expect_equal(mixture_v_spark$method$fit$args,
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
  expect_equal(translate(glm_fam, engine = "glm")$method$fit$args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 family = quote(binomial(link = "probit"))
               )
  )

  glmnet_nlam <- logistic_reg(others = list(nlambda = 10))
  expect_equal(translate(glmnet_nlam, engine = "glmnet")$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 nlambda = 10,
                 family = "binomial"
               )
  )

  stan_samp <- logistic_reg(others = list(chains = 1, iter = 5))
  expect_equal(translate(stan_samp, engine = "stan")$method$fit$args,
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
  expect_equal(translate(spark_iter, engine = "spark")$method$fit$args,
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

  expr3     <- logistic_reg(mixture = 0, penalty = varying())
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
  expect_error(logistic_reg(penalty = -1))
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
lc_basic <- logistic_reg()
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
      engine = "glm",
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- fit(
      lc_basic,
      funded_amnt ~ term,
      data = lending_club,
      engine = "glm",
      control = ctrl
    )
  )

  # passes interactively but not on R CMD check
  # glm_form_catch <- fit(
  #   lc_basic,
  #   funded_amnt ~ term,
  #   data = lending_club,
  #   engine = "glm",
  #   control = caught_ctrl
  # )
  # expect_true(inherits(glm_form_catch$fit, "try-error"))

  glm_xy_catch <- fit_xy(
    lc_basic,
    engine = "glm",
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
    engine = "glm",
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
    engine = "glm",
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
    logistic_reg(),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    engine = "glm",
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

