library(testthat)
context("linear regression execution with glmnet")
library(parsnip)
library(rlang)

###################################################################

num_pred <- c("Sepal.Width", "Petal.Width", "Petal.Length")
iris_bad_form <- as.formula(Species ~ term)
iris_basic <- linear_reg(penalty = .1, mixture = .3)
no_lambda <- linear_reg(mixture = .3)
ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

test_that('glmnet execution', {

  skip_if_not_installed("glmnet")

  expect_error(
    fit_xy(
      iris_basic,
      engine = "glmnet",
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
      engine = "glm",
      control = ctrl
    )
  )

  glmnet_xy_catch <- fit_xy(
    iris_basic,
    x = iris[, num_pred],
    y = factor(iris$Sepal.Length),
    engine = "glmnet",
    control = caught_ctrl
  )
  expect_true(inherits(glmnet_xy_catch$fit, "try-error"))

})

test_that('glmnet prediction, single lambda', {

  skip_if_not_installed("glmnet")

  res_xy <- fit_xy(
    iris_basic,
    engine = "glmnet",
    control = ctrl,
    x = iris[, num_pred],
    y = iris$Sepal.Length
  )

  uni_pred <-
    predict(res_xy$fit,
            newx = as.matrix(iris[1:5, num_pred]),
            s = 0.1)
  uni_pred <- unname(uni_pred[,1])

  expect_equal(
    safepredict::as_pred_tibble(uni_pred),
    predict(res_xy, iris[1:5, num_pred], penalty = 0.1)
  )

  res_form <- fit(
    iris_basic,
    Sepal.Length ~ log(Sepal.Width) + Species,
    data = iris,
    engine = "glmnet",
    control = ctrl
  )

  form_pred <- model.matrix(Sepal.Length ~ log(Sepal.Width) + Species, data = iris)
  form_pred <- form_pred[1:5, -1]

  form_pred <-
    predict(res_form$fit,
            newx = form_pred,
            s = 0.1)
  form_pred <- unname(form_pred[,1])
  expect_equal(
    safepredict::as_pred_tibble(form_pred),
    predict(res_form, iris[1:5, c("Sepal.Width", "Species")], penalty = 0.1))
})


# for now this will just check that arguments are appropriately passed to
# safe_predict. correctness checks will live in safe_predict itself.

test_that('glmnet prediction, multiple lambda', {

  skip_if_not_installed("glmnet")

  iris_mult <- linear_reg(penalty = c(.01, 0.1), mixture = .3)

  res_xy <- fit_xy(
    iris_mult,
    engine = "glmnet",
    control = ctrl,
    x = iris[, num_pred],
    y = iris$Sepal.Length
  )

  mpreds <- multi_predict(res_xy, iris[1:5, num_pred], type = "response")
  obs_1_pred <- mpreds$.pred[[1]]

  # default case: one prediction at each value of lambda
  expect_equal(nrow(obs_1_pred), length(res_xy$fit$lambda))

  expect_error(
    multi_predict(res_xy, iris[1:5, num_pred], type = "class")
  )

  res_form <- fit(
    iris_mult,
    Sepal.Length ~ log(Sepal.Width) + Species,
    data = iris,
    engine = "glmnet",
    control = ctrl
  )
})

test_that('submodel prediction', {

  reg_fit <-
    linear_reg() %>%
    fit(mpg ~ ., data = mtcars[-(1:4), ], engine = "glmnet")

  pred_glmn <- predict(reg_fit$fit, as.matrix(mtcars[1:4, -1]), s = .1)

  mp_res <- multi_predict(reg_fit, new_data = mtcars[1:4, -1], params = .1)
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred"]], unname(pred_glmn[,1]))
})

