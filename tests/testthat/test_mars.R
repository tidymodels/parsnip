library(testthat)
library(parsnip)
library(rlang)

# ------------------------------------------------------------------------------

context("mars tests")
source("helpers.R")

# ------------------------------------------------------------------------------

test_that('primary arguments', {
  basic <- mars(mode = "regression")
  basic_mars <- translate(basic %>% set_engine("earth"))
  expect_equal(basic_mars$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 keepxy = TRUE
               )
  )

  num_terms <- mars(num_terms = 4, mode = "classification")
  num_terms_mars <- translate(num_terms %>% set_engine("earth"))
  expect_equal(num_terms_mars$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 nprune = new_empty_quosure(4),
                 glm = expr(list(family = stats::binomial)),
                 keepxy = TRUE
               )
  )

  prod_degree <- mars(prod_degree = 1, mode = "regression")
  prod_degree_mars <- translate(prod_degree %>% set_engine("earth"))
  expect_equal(prod_degree_mars$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 degree = new_empty_quosure(1),
                 keepxy = TRUE
               )
  )

  prune_method_v <- mars(prune_method = varying(), mode = "regression")
  prune_method_v_mars <- translate(prune_method_v %>% set_engine("earth"))
  expect_equal(prune_method_v_mars$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 pmethod = new_empty_quosure(varying()),
                 keepxy = TRUE
               )
  )
})

test_that('engine arguments', {
  mars_keep <- mars(mode = "regression")
  expect_equal(translate(mars_keep %>% set_engine("earth", keepxy = FALSE))$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 keepxy = new_empty_quosure(FALSE)
               )
  )
})


test_that('updating', {
  expr1     <- mars() %>% set_engine("earth", model = FALSE)
  expr1_exp <- mars(num_terms = 1) %>% set_engine("earth", model = FALSE)

  expr2     <- mars(num_terms = varying()) %>% set_engine("earth")
  expr2_exp <- mars(num_terms = varying()) %>% set_engine("earth", nk = 10)

  expr3     <- mars(num_terms = 1, prod_degree = varying()) %>% set_engine("earth")
  expr3_exp <- mars(num_terms = 1) %>% set_engine("earth")

  expr4     <- mars(num_terms = 0) %>% set_engine("earth", nk = 10)
  expr4_exp <- mars(num_terms = 0) %>% set_engine("earth", nk = 10, trace = 2)

  expr5     <- mars(num_terms = 1) %>% set_engine("earth", nk = 10)
  expr5_exp <- mars(num_terms = 1) %>% set_engine("earth", nk = 10, trace = 2)

  expect_equal(update(expr1, num_terms = 1), expr1_exp)
  expect_equal(update(expr3, num_terms = 1, fresh = TRUE), expr3_exp)
})

test_that('bad input', {
  # expect_error(mars(prod_degree = -1))
  # expect_error(mars(num_terms = -1))
  expect_error(translate(mars() %>% set_engine("wat?")))
  expect_error(translate(mars(mode = "regression") %>% set_engine()))
  expect_error(translate(mars(formula = y ~ x)))
  expect_warning(
    translate(
      mars(mode = "regression") %>% set_engine("earth", x = iris[,1:3], y = iris$Species)
      )
  )
})

# ------------------------------------------------------------------------------

num_pred <- c("Sepal.Width", "Petal.Width", "Petal.Length")
iris_bad_form <- as.formula(Species ~ term)
iris_basic <- mars(mode = "regression") %>% set_engine("earth")

ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

# ------------------------------------------------------------------------------

test_that('mars execution', {
  skip_if_not_installed("earth")

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
  parsnip:::load_libs(res, attach = TRUE)

})

test_that('mars prediction', {
  skip_if_not_installed("earth")

  uni_pred <- c(5.02371514510488, 4.70502120747471, 4.78973285129011, 4.81152592623742,
                5.08745393263092)
  inl_pred <- c(5.07584328502019, 4.64927636051174, 4.82786784324037, 4.74001260567429,
                5.15379794835255)
  mv_pred <-
    structure(
      list(Sepal.Width =
             c(3.4874092243636, 3.34173526636919, 3.17647644756747, 3.14280919018489, 3.41457224536639),
           Petal.Width =
             c(0.237414046784062, 0.221455118452782, 0.18348960240454, 0.219523313672823, 0.229434582618422
             )), class = "data.frame", row.names = c(NA, -5L))

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


test_that('submodel prediction', {
  skip_if_not_installed("earth")

  reg_fit <-
    mars(
      num_terms = 20,
      mode = "regression",
      prune_method = "none"
    ) %>%
    set_engine("earth", keepxy = TRUE) %>%
    fit(mpg ~ ., data = mtcars[-(1:4), ])

  parsnip:::load_libs(reg_fit$spec, quiet = TRUE, attach = TRUE)
  tmp_reg <- reg_fit$fit
  tmp_reg$call[["pmethod"]] <- eval_tidy(tmp_reg$call[["pmethod"]])
  tmp_reg$call[["keepxy"]]  <- eval_tidy(tmp_reg$call[["keepxy"]])
  tmp_reg$call[["nprune"]]  <- eval_tidy(tmp_reg$call[["nprune"]])


  pruned_reg <- update(tmp_reg, nprune = 5)
  pruned_reg_pred <- predict(pruned_reg, mtcars[1:4, -1])[,1]

  mp_res <- multi_predict(reg_fit, new_data = mtcars[1:4, -1], num_terms = 5)
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred"]], pruned_reg_pred)

  vars <- c("female", "tenure", "total_charges", "phone_service", "monthly_charges")
  class_fit <-
    mars(mode = "classification", prune_method = "none")  %>%
    set_engine("earth", keepxy = TRUE) %>%
    fit(churn ~ .,
        data = wa_churn[-(1:4), c("churn", vars)])

  cls_fit <- class_fit$fit
  cls_fit$call[["pmethod"]] <- eval_tidy(cls_fit$call[["pmethod"]])
  cls_fit$call[["keepxy"]]  <- eval_tidy(cls_fit$call[["keepxy"]])

  pruned_cls <- update(cls_fit, nprune = 5)
  pruned_cls_pred <- predict(pruned_cls, wa_churn[1:4, vars], type = "response")[,1]

  mp_res <- multi_predict(class_fit, new_data = wa_churn[1:4, vars], num_terms = 5, type = "prob")
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred_No"]], pruned_cls_pred)

  expect_error(
    multi_predict(reg_fit, newdata = mtcars[1:4, -1], num_terms = 5),
    "Did you mean"
  )
})


# ------------------------------------------------------------------------------

data("lending_club")

test_that('classification', {
  skip_if_not_installed("earth")

  expect_error(
    glm_mars <- mars(mode = "classification")  %>%
      set_engine("earth") %>%
      fit(Class ~ ., data = lending_club[-(1:5),]),
    regexp = NA
  )
  expect_true(!is.null(glm_mars$fit$glm.list))
  parsnip_pred <- predict_classprob(glm_mars, new_data = lending_club[1:5, -ncol(lending_club)])

  earth_pred <-
    c(0.95631355972526, 0.971917781277731, 0.894245392500336, 0.962667553751077,
      0.985827594261896)

  expect_equal(parsnip_pred[["good"]], earth_pred)
})

