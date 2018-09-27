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
                 weights = quote(missing_arg()),
                 keepxy = TRUE
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
                 glm = quote(list(family = stats::binomial)),
                 keepxy = TRUE
               )
  )

  prod_degree <- mars(prod_degree = 1, mode = "regression")
  prod_degree_mars <- translate(prod_degree, engine = "earth")
  expect_equal(prod_degree_mars$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 degree = 1,
                 keepxy = TRUE
               )
  )

  prune_method_v <- mars(prune_method = varying(), mode = "regression")
  prune_method_v_mars <- translate(prune_method_v, engine = "earth")
  expect_equal(prune_method_v_mars$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 pmethod = varying(),
                 keepxy = TRUE
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

test_that('mars prediction', {

  skip_if_not_installed("earth")
  library(earth)

  uni_mars <- earth(Sepal.Length ~ Sepal.Width + Petal.Width + Petal.Length, data = iris)
  uni_pred <- unname(predict(uni_mars, newdata = iris[1:5, ])[,1])
  inl_mars <- earth(Sepal.Length ~ log(Sepal.Width) + Species, data = iris)
  inl_pred <- unname(predict(inl_mars, newdata = iris[1:5, ])[,1])
  mv_mars <- earth(cbind(Sepal.Width, Petal.Width) ~ ., data = iris)
  mv_pred <- as.data.frame(predict(mv_mars, newdata = iris[1:5, ]))

  res_xy <- fit_xy(
    iris_basic,
    x = iris[, num_pred],
    y = iris$Sepal.Length,
    engine = "earth",
    control = ctrl
  )

  expect_equal(uni_pred, predict_num(res_xy, iris[1:5, num_pred]))

  res_form <- fit(
    iris_basic,
    Sepal.Length ~ log(Sepal.Width) + Species,
    data = iris,
    engine = "earth",
    control = ctrl
  )
  expect_equal(inl_pred, predict_num(res_form, iris[1:5, ]))

  res_mv <- fit(
    iris_basic,
    cbind(Sepal.Width, Petal.Width) ~ .,
    data = iris,
    control = ctrl,
    engine = "earth"
  )
  expect_equal(mv_pred, predict_num(res_mv, iris[1:5,]))
})


test_that('submodel prediction', {

  skip_if_not_installed("earth")
  library(earth)

  reg_fit <-
    mars(
      num_terms = 20,
      prune_method = "none",
      mode = "regression",
      others = list(keepxy = TRUE)
    ) %>%
    fit(mpg ~ ., data = mtcars[-(1:4), ], engine = "earth")

  pruned_fit <- update(reg_fit$fit, nprune = 5)
  pruned_pred <- predict(pruned_fit, mtcars[1:4, -1])[,1]

  mp_res <- multi_predict(reg_fit, new_data = mtcars[1:4, -1], num_terms = 5)
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred"]], pruned_pred)

  vars <- c("female", "tenure", "total_charges", "phone_service", "monthly_charges")
  class_fit <-
    mars(mode = "classification", prune_method = "none", others = list(keepxy = TRUE)) %>%
    fit(churn ~ .,
        data = wa_churn[-(1:4), c("churn", vars)],
        engine = "earth")

  pruned_fit <- update(class_fit$fit, nprune = 5)
  pruned_pred <- predict(pruned_fit, wa_churn[1:4, vars], type = "response")[,1]

  mp_res <- multi_predict(class_fit, new_data = wa_churn[1:4, vars], num_terms = 5, type = "prob")
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred_No"]], pruned_pred)
})


###################################################################

data("lending_club")

test_that('classification', {

  skip_if_not_installed("earth")

  expect_error(
    glm_mars <- mars(mode = "classification") %>%
      fit(Class ~ ., data = lending_club[-(1:5),], engine = "earth"),
    regexp = NA
  )
  expect_true(!is.null(glm_mars$fit$glm.list))
  parsnip_pred <- predict_classprob(glm_mars, new_data = lending_club[1:5, -ncol(lending_club)])

  library(earth)
  earth_fit <- earth(Class ~ ., data = lending_club[-(1:5),],
                     glm = list(family = binomial))
  earth_pred <-
    predict(
      earth_fit,
      newdata = lending_club[1:5, -ncol(lending_club)],
      type = "response"
    )

  expect_equal(parsnip_pred[["good"]], earth_pred[,1])
})

