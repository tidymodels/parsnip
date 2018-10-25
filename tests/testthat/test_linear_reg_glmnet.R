library(testthat)
library(parsnip)
library(rlang)

# ------------------------------------------------------------------------------

context("linear regression execution with glmnet")

num_pred <- c("Sepal.Width", "Petal.Width", "Petal.Length")
iris_bad_form <- as.formula(Species ~ term)
iris_basic <- linear_reg(penalty = .1, mixture = .3) %>%
  set_engine("glmnet", nlambda = 15)
no_lambda <- linear_reg(mixture = .3) %>%
  set_engine("glmnet")

ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

# ------------------------------------------------------------------------------

test_that('glmnet execution', {

  skip_if_not_installed("glmnet")

  expect_error(
    fit_xy(
      iris_basic,
      control = ctrl,
      x = iris[, num_pred],
      y = iris$Sepal.Length
    ),
    regexp = NA
  )

  expect_error(
    fit(
      iris_basic,
      iris_bad_form,
      data = iris,
      control = ctrl
    )
  )

  glmnet_xy_catch <- fit_xy(
    iris_basic,
    x = iris[, num_pred],
    y = factor(iris$Sepal.Length),
    control = caught_ctrl
  )
  expect_true(inherits(glmnet_xy_catch$fit, "try-error"))

})

test_that('glmnet prediction, single lambda', {

  skip_if_not_installed("glmnet")

  res_xy <- fit_xy(
    iris_basic,
    control = ctrl,
    x = iris[, num_pred],
    y = iris$Sepal.Length
  )

  uni_pred <-
    predict(res_xy$fit,
            newx = as.matrix(iris[1:5, num_pred]),
            s = iris_basic$spec$args$penalty)
  uni_pred <- unname(uni_pred[,1])

  expect_equal(uni_pred, predict_numeric(res_xy, iris[1:5, num_pred]))

  res_form <- fit(
    iris_basic,
    Sepal.Length ~ log(Sepal.Width) + Species,
    data = iris,
    control = ctrl
  )

  form_pred <- model.matrix(Sepal.Length ~ log(Sepal.Width) + Species, data = iris)
  form_pred <- form_pred[1:5, -1]

  form_pred <-
    predict(res_form$fit,
            newx = form_pred,
            s = res_form$spec$spec$args$penalty)
  form_pred <- unname(form_pred[,1])

  expect_equal(form_pred, predict_numeric(res_form, iris[1:5, c("Sepal.Width", "Species")]))
})


test_that('glmnet prediction, multiple lambda', {

  skip_if_not_installed("glmnet")

  lams <- c(.01, 0.1)

  iris_mult <- linear_reg(penalty = lams, mixture = .3) %>%
    set_engine("glmnet")

  res_xy <- fit_xy(
    iris_mult,
    control = ctrl,
    x = iris[, num_pred],
    y = iris$Sepal.Length
  )

  mult_pred <-
    predict(res_xy$fit,
            newx = as.matrix(iris[1:5, num_pred]),
            s = lams)
  mult_pred <- stack(as.data.frame(mult_pred))
  mult_pred$lambda <- rep(lams, each = 5)
  mult_pred <- mult_pred[,-2]

  expect_equal(mult_pred, predict_numeric(res_xy, iris[1:5, num_pred]))

  res_form <- fit(
    iris_mult,
    Sepal.Length ~ log(Sepal.Width) + Species,
    data = iris,
    control = ctrl
  )

  form_mat <- model.matrix(Sepal.Length ~ log(Sepal.Width) + Species, data = iris)
  form_mat <- form_mat[1:5, -1]

  form_pred <-
    predict(res_form$fit,
            newx = form_mat,
            s = lams)
  form_pred <- stack(as.data.frame(form_pred))
  form_pred$lambda <- rep(lams, each = 5)
  form_pred <- form_pred[,-2]

  expect_equal(form_pred, predict_numeric(res_form, iris[1:5, c("Sepal.Width", "Species")]))
})

test_that('glmnet prediction, all lambda', {

  skip_if_not_installed("glmnet")

  iris_all <- linear_reg(mixture = .3) %>%
    set_engine("glmnet")

  res_xy <- fit_xy(
    iris_all,
    control = ctrl,
    x = iris[, num_pred],
    y = iris$Sepal.Length
  )

  all_pred <- predict(res_xy$fit, newx = as.matrix(iris[1:5, num_pred]))
  all_pred <- stack(as.data.frame(all_pred))
  all_pred$lambda <- rep(res_xy$fit$lambda, each = 5)
  all_pred <- all_pred[,-2]

  expect_equal(all_pred, predict_numeric(res_xy, iris[1:5, num_pred]))

  # test that the lambda seq is in the right order (since no docs on this)
  tmp_pred <- predict(res_xy$fit, newx = as.matrix(iris[1:5, num_pred]),
                      s = res_xy$fit$lambda[5])[,1]
  expect_equal(all_pred$values[all_pred$lambda == res_xy$fit$lambda[5]],
               unname(tmp_pred))

  res_form <- fit(
    iris_all,
    Sepal.Length ~ log(Sepal.Width) + Species,
    data = iris,
    control = ctrl
  )

  form_mat <- model.matrix(Sepal.Length ~ log(Sepal.Width) + Species, data = iris)
  form_mat <- form_mat[1:5, -1]

  form_pred <- predict(res_form$fit, newx = form_mat)
  form_pred <- stack(as.data.frame(form_pred))
  form_pred$lambda <- rep(res_form$fit$lambda, each = 5)
  form_pred <- form_pred[,-2]

  expect_equal(form_pred, predict_numeric(res_form, iris[1:5, c("Sepal.Width", "Species")]))
})


test_that('submodel prediction', {

  skip_if_not_installed("glmnet")

  reg_fit <-
    linear_reg() %>%
    set_engine("glmnet") %>%
    fit(mpg ~ ., data = mtcars[-(1:4), ])

  pred_glmn <- predict(reg_fit$fit, as.matrix(mtcars[1:4, -1]), s = .1)

  mp_res <- multi_predict(reg_fit, new_data = mtcars[1:4, -1], penalty = .1)
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred"]], unname(pred_glmn[,1]))
  
  expect_error(
    multi_predict(reg_fit, newdata = mtcars[1:4, -1], penalty = .1), 
    "Did you mean"
  )
})

