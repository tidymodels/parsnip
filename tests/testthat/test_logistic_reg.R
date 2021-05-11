library(testthat)
library(parsnip)
library(rlang)
library(tibble)

# ------------------------------------------------------------------------------

context("logistic regression")
source(test_path("helpers.R"))
source(test_path("helper-objects.R"))
hpc <- hpc_data[1:150, c(2:5, 8)]


# ------------------------------------------------------------------------------

test_that('primary arguments', {
  basic <- logistic_reg()
  basic_glm <- translate(basic %>% set_engine("glm"))
  basic_glmnet <- translate(basic %>% set_engine("glmnet"))
  basic_liblinear <- translate(basic %>% set_engine("LiblineaR"))
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
  expect_equal(basic_liblinear$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 wi = expr(missing_arg()),
                 verbose = FALSE
               )
  )
  expect_equal(basic_stan$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 family = expr(stats::binomial),
                 refresh = 0
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
  penalty_liblinear <- translate(penalty %>% set_engine("LiblineaR"))
  penalty_spark <- translate(penalty %>% set_engine("spark"))
  expect_equal(penalty_glmnet$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 family = "binomial"
               )
  )
  expect_equal(penalty_liblinear$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 wi = expr(missing_arg()),
                 cost = new_empty_quosure(1),
                 verbose = FALSE
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
  mixture_v_liblinear <- translate(mixture_v %>% set_engine("LiblineaR"))
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
  expect_equal(mixture_v_liblinear$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 wi = expr(missing_arg()),
                 type = new_empty_quosure(varying()),
                 verbose = FALSE
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

  penalty_v <- logistic_reg(penalty = 1)
  penalty_v_glmnet <- translate(penalty_v %>% set_engine("glmnet"))
  penalty_v_liblinear <- translate(penalty_v %>% set_engine("LiblineaR"))
  penalty_v_spark <- translate(penalty_v %>% set_engine("spark"))
  expect_equal(penalty_v_glmnet$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 family = "binomial"
               )
  )
  expect_equal(penalty_v_liblinear$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 wi = expr(missing_arg()),
                 cost = new_empty_quosure(1),
                 verbose = FALSE
               )
  )
  expect_equal(penalty_v_spark$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 formula = expr(missing_arg()),
                 weight_col = expr(missing_arg()),
                 reg_param = new_empty_quosure(1),
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

  liblinear_bias <- logistic_reg()
  expect_equal(
    translate(liblinear_bias %>% set_engine("LiblineaR", bias = 0))$method$fit$args,
    list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      wi = expr(missing_arg()),
      bias = new_empty_quosure(0),
      verbose = FALSE
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
      family = expr(stats::binomial),
      refresh = 0
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

  # For issue #431
  with_path <-
    logistic_reg(penalty = 1) %>%
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
  expr1     <- logistic_reg() %>%
    set_engine("glm", family = expr(binomial(link = "probit")))
  expr1_exp <- logistic_reg(mixture = 0) %>%
    set_engine("glm", family = expr(binomial(link = "probit")))

  expr2     <- logistic_reg(mixture = varying()) %>% set_engine("glmnet", nlambda = varying())
  expr2_exp <- logistic_reg(mixture = varying()) %>% set_engine("glmnet", nlambda = 10)

  expr3     <- logistic_reg(mixture = 0, penalty = varying()) %>% set_engine("glmnet", nlambda = varying())
  expr3_exp <- logistic_reg(mixture = 1) %>% set_engine("glmnet", nlambda = 10)

  expr4     <- logistic_reg(mixture = 0) %>% set_engine("glmnet", nlambda = 10)
  expr4_exp <- logistic_reg(mixture = 0) %>% set_engine("glmnet", nlambda = 10, pmax = 2)

  expr5     <- logistic_reg(mixture = 1) %>% set_engine("glmnet", nlambda = 10)
  expr5_exp <- logistic_reg(mixture = 1) %>% set_engine("glmnet", nlambda = 10, pmax = 2)

  expect_equal(update(expr1, mixture = 0), expr1_exp)
  expect_equal(update(expr2, nlambda = 10), expr2_exp)
  expect_equal(update(expr3, mixture = 1, fresh = TRUE, nlambda = 10), expr3_exp)

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
  expect_error(logistic_reg(mode = "regression"))
  expect_error(translate(logistic_reg(formula = y ~ x)))
  expect_error(translate(logistic_reg(x = hpc[,1:3], y = hpc$class) %>% set_engine(engine = "glmnet")))
  expect_error(translate(logistic_reg(formula = y ~ x) %>% set_engine(engine = "glm")))
  expect_error(translate(logistic_reg(mixture = 0.5) %>% set_engine(engine = "LiblineaR")))
})

# ------------------------------------------------------------------------------

lending_club <- head(lending_club, 200)
lc_form <- as.formula(Class ~ log(funded_amnt) + int_rate)
num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")
lc_basic <- logistic_reg() %>% set_engine("glm")
ll_basic <- logistic_reg() %>% set_engine("LiblineaR")

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
  expect_output(print(res), "parsnip model object")

  expect_error(
    res <- fit(
      lc_basic,
      funded_amnt ~ term,
      data = lending_club,
      control = ctrl
    )
  )

  # wrong outcome type
  expect_error(
    glm_form_catch <- fit(
      lc_basic,
      funded_amnt ~ term,
      data = lending_club,
      control = caught_ctrl
    )
  )

  expect_error(
    glm_xy_catch <- fit_xy(
      lc_basic,
      control = caught_ctrl,
      x = lending_club[, num_pred],
      y = lending_club$total_bal_il
    )
  )
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
  expect_equal(xy_pred, predict(classes_xy, lending_club[1:7, num_pred], type = "class")$.pred_class)

})

test_that('glm probabilities', {
  classes_xy <- fit_xy(
    lc_basic,
    x = lending_club[, num_pred],
    y = lending_club$Class,
    control = ctrl
  )

  xy_pred <- unname(predict(classes_xy$fit,
                            newdata = lending_club[1:7, num_pred],
                            type = "response"))
  xy_pred <- tibble(.pred_bad = 1 - xy_pred, .pred_good = xy_pred)
  expect_equal(xy_pred, predict(classes_xy, lending_club[1:7, num_pred], type = "prob"))

  one_row <- predict(classes_xy, lending_club[1, num_pred], type = "prob")
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

  expect_equivalent(confidence_parsnip$.pred_lower_good, lower_glm)
  expect_equivalent(confidence_parsnip$.pred_upper_good, upper_glm)
  expect_equivalent(confidence_parsnip$.pred_lower_bad, 1 - upper_glm)
  expect_equivalent(confidence_parsnip$.pred_upper_bad, 1 - lower_glm)
  expect_equivalent(confidence_parsnip$.std_error, pred_glm$se.fit)

})

test_that('liblinear execution', {

  skip_if_not_installed("LiblineaR")

  expect_error(
    res <- fit_xy(
      ll_basic,
      x = lending_club[, num_pred],
      y = lending_club$Class,
      control = ctrl
    ),
    regexp = NA
  )
  expect_output(print(res), "parsnip model object")

  expect_error(
    res <- fit(
      ll_basic,
      funded_amnt ~ term,
      data = lending_club,
      control = ctrl
    )
  )

  expect_error(
    tidy_res <- tidy(res),
    NA
  )
  expect_s3_class(tidy_res, "tbl_df")
  expect_equal(colnames(tidy_res), c("term", "estimate"))

  # wrong outcome type
  expect_error(
    glm_form_catch <- fit(
      ll_basic,
      funded_amnt ~ term,
      data = lending_club,
      control = caught_ctrl
    )
  )

  expect_error(
    glm_xy_catch <- fit_xy(
      ll_basic,
      control = caught_ctrl,
      x = lending_club[, num_pred],
      y = lending_club$total_bal_il
    )
  )


})

test_that('liblinear prediction', {

  skip_if_not_installed("LiblineaR")

  classes_xy <- fit_xy(
    ll_basic,
    x = lending_club[, num_pred],
    y = lending_club$Class,
    control = ctrl
  )

  xy_pred <- predict(classes_xy$fit, newx = lending_club[1:7, num_pred])
  xy_pred <- xy_pred$predictions
  expect_equal(xy_pred, predict(classes_xy, lending_club[1:7, num_pred], type = "class")$.pred_class)

})

test_that('liblinear probabilities', {

  skip_if_not_installed("LiblineaR")

  classes_xy <- fit_xy(
    ll_basic,
    x = lending_club[, num_pred],
    y = lending_club$Class,
    control = ctrl
  )

  xy_pred <- predict(classes_xy$fit,
                     newx = lending_club[1:7, num_pred],
                     proba = TRUE)
  xy_pred <- as_tibble(xy_pred$probabilities)
  xy_pred <- tibble(.pred_good = xy_pred$good,
                    .pred_bad  = xy_pred$bad)
  expect_equal(xy_pred, predict(classes_xy, lending_club[1:7, num_pred], type = "prob"))

  one_row <- predict(classes_xy, lending_club[1, num_pred], type = "prob")
  expect_equal(xy_pred[1,], one_row)

})


test_that('default engine', {
  expect_warning(
    fit <- logistic_reg() %>% fit(Class ~ log(funded_amnt) + int_rate, data = lending_club),
    "Engine set to"
  )
  expect_true(inherits(fit$fit, "glm"))
})
