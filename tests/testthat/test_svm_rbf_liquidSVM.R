library(testthat)
library(parsnip)
library(rlang)

# ------------------------------------------------------------------------------

context("poly SVM")
source("helpers.R")

# ------------------------------------------------------------------------------

test_that('primary arguments', {
  basic <- svm_rbf()
  basic_liquidSVM <- translate(basic %>% set_engine("liquidSVM"))

  expect_equal(
    object = basic_liquidSVM$method$fit$args,
    expected = list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      folds = 1
    )
  )

  rbf_sigma <- svm_rbf(rbf_sigma = .2)
  rbf_sigma_liquidSVM <- translate(rbf_sigma %>% set_engine("liquidSVM"))

  expect_equal(
    object = rbf_sigma_liquidSVM$method$fit$args,
    expected = list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      folds = 1,
      gammas = quo(1 / !!new_empty_quosure(.2))
    )
  )

})

test_that('engine arguments', {

  liquidSVM_scale <-
    svm_rbf() %>%
    set_engine("liquidSVM", scale = FALSE, predict.prob = TRUE, threads = 2, gpus = 1)

  expect_equal(
    object = translate(liquidSVM_scale, "liquidSVM")$method$fit$args,
    expected = list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      scale = new_empty_quosure(FALSE),
      predict.prob = new_empty_quosure(TRUE),
      threads = new_empty_quosure(2),
      gpus = new_empty_quosure(1),
      folds = 1
    )
  )

})


test_that('updating', {

  expr1     <- svm_rbf()  %>% set_engine("liquidSVM", scale = TRUE)
  expr1_exp <- svm_rbf(rbf_sigma = .1) %>% set_engine("liquidSVM", scale = TRUE)

  expr3     <- svm_rbf(rbf_sigma = .2) %>% set_engine("liquidSVM")
  expr3_exp <- svm_rbf(rbf_sigma = .3) %>% set_engine("liquidSVM")

  expect_equal(update(expr1, rbf_sigma = .1), expr1_exp)
  expect_equal(update(expr3, rbf_sigma = .3, fresh = TRUE), expr3_exp)
})

test_that('bad input', {
  expect_error(svm_rbf(mode = "reallyunknown"))
  expect_error(translate(svm_rbf() %>% set_engine( NULL)))
})

# ------------------------------------------------------------------------------
# define model specification for classification and regression

reg_mod <-
  svm_rbf(rbf_sigma = .1, cost = 0.25) %>%
  set_engine("liquidSVM", random_seed = 1234) %>%
  set_mode("regression")

cls_mod <-
  svm_rbf(rbf_sigma = .1, cost = 0.125) %>%
  set_engine("liquidSVM", random_seed = 1234) %>%
  set_mode("classification")

ctrl <- fit_control(verbosity = 0, catch = FALSE)

# ------------------------------------------------------------------------------

test_that('svm rbf regression', {

  skip_if_not_installed("liquidSVM")

  expect_error(
    fit_xy(
      reg_mod,
      control = ctrl,
      x = iris[, 2:4],
      y = iris$Sepal.Length
    ),
    regexp = NA
  )

  expect_error(
    fit(
      reg_mod,
      Sepal.Length ~ .,
      data = iris[, -5],
      control = ctrl
    ),
    regexp = NA
  )

})


test_that('svm rbf regression prediction', {

  skip_if_not_installed("liquidSVM")

  reg_form <-
    fit(
      object = reg_mod,
      formula = Sepal.Length ~ .,
      data = iris[, -5],
      control = ctrl
    )

  reg_xy_form <-
    fit_xy(
      object = reg_mod,
      x = iris[, 2:4],
      y = iris$Sepal.Length,
      control = ctrl
    )
  expect_equal(reg_form$spec, reg_xy_form$spec)

  liquidSVM_form <-
    liquidSVM::svm(
      x = Sepal.Length ~ .,
      y = iris[, -5],
      gammas = 1/.1,
      lambdas = 0.25,
      folds = 1,
      random_seed = 1234
    )

  liquidSVM_xy_form <-
    liquidSVM::svm(
      x = iris[, 2:4],
      y = iris$Sepal.Length,
      gammas = 1/.1,
      lambdas = 0.25,
      folds = 1,
      random_seed = 1234
    )

  # check coeffs for liquidSVM formula and liquidSVM xy fit interfaces
  expect_equal(liquidSVM::getSolution(liquidSVM_form)[c("coeff", "sv")],
               liquidSVM::getSolution(liquidSVM_xy_form)[c("coeff", "sv")])

  # check predictions for liquidSVM formula and liquidSVM xy interfaces
  liquidSVM_form_preds <- predict(liquidSVM_form, iris[1:3, 2:4])
  liquidSVM_form_xy_preds <- predict(liquidSVM_xy_form, iris[1:3, 2:4])
  expect_equal(liquidSVM_form_preds, liquidSVM_form_xy_preds)

  # check predictions for parsnip formula and liquidSVM formula interfaces
  liquidSVM_pred <-
    structure(
      list(.pred = liquidSVM_form_preds),
      row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))

  parsnip_pred <- predict(reg_form, iris[1:3, 2:4])
  expect_equal(as.data.frame(liquidSVM_pred), as.data.frame(parsnip_pred))

  # check that coeffs are equal for formula methods called via parsnip and liquidSVM
  expect_equal(liquidSVM::getSolution(reg_form$fit)[c("coeff", "sv")],
               liquidSVM::getSolution(liquidSVM_form)[c("coeff", "sv")])

  # check coeffs are equivalent for parsnip fit_xy and parsnip formula methods
  expect_equal(liquidSVM::getSolution(reg_form$fit)[c("coeff", "sv")],
               liquidSVM::getSolution(reg_xy_form$fit)[c("coeff", "sv")])

  # check predictions are equal for parsnip xy and liquidSVM xy methods
  parsnip_xy_pred <- predict(reg_xy_form, iris[1:3, -c(1, 5)])
  expect_equal(as.data.frame(liquidSVM_pred), as.data.frame(parsnip_xy_pred))
})


# ------------------------------------------------------------------------------

test_that('svm rbf classification', {

  skip_if_not_installed("kernlab")

  expect_error(
    fit_xy(
      cls_mod,
      control = ctrl,
      x = iris[, -5],
      y = iris$Species
    ),
    regexp = NA
  )

  expect_error(
    fit(
      cls_mod,
      Species ~ .,
      data = iris,
      control = ctrl
    ),
    regexp = NA
  )

})


test_that('svm rbf classification probabilities', {

  skip_if_not_installed("liquidSVM")

  ind <- c(1, 51, 101)

  # check that parsnip and liquidSVM formula interfaces produce same predicted classes
  cls_form <-
    fit(
      cls_mod,
      Species ~ .,
      data = iris,
      control = ctrl
    )

  liquidSVM_cls_form <-
    liquidSVM::svm(
      x = Species ~ .,
      y = iris,
      gammas = 1/.1,
      lambdas = 0.125,
      folds = 1,
      random_seed = 1234
    )

  liquidSVM_class <-
    structure(list(.pred_class = predict(liquidSVM_cls_form, iris)[ind]),
      row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))

  parsnip_class <- predict(cls_form, iris[ind, -5])
  expect_equal(liquidSVM_class, parsnip_class)

  # check that parsnip xy interface and liquidSVM formula interface produce same model
  cls_xy_form <-
    fit_xy(
      cls_mod,
      x = iris[, 1:4],
      y = iris$Species,
      control = ctrl
    )
  expect_equal(liquidSVM::getSolution(cls_form$fit)[c("coeff", "sv")],
               liquidSVM::getSolution(cls_xy_form$fit)[c("coeff", "sv")])

  # check probabilities
  cls_mod <-
    svm_rbf(rbf_sigma = .1, cost = 0.125) %>%
    set_engine("liquidSVM", random_seed = 1234, predict.prob = TRUE) %>%
    set_mode("classification")

  cls_form <-
    fit(
      cls_mod,
      Species ~ .,
      data = iris,
      control = ctrl
    )

  cls_xy_form <-
    fit_xy(
      cls_mod,
      x = iris[, -5],
      y = iris$Species,
      control = ctrl
    )

  liquidSVM_cls_form <-
    liquidSVM::svm(
      x = Species ~ .,
      y = iris,
      gammas = 1/.1,
      lambdas = 0.125,
      folds = 1,
      random_seed = 1234,
      predict.prob = TRUE
    )

  liquidSVM_probs <-
    predict(liquidSVM_cls_form, iris[ind, -5], predict.prob = TRUE) %>%
    as_tibble() %>%
    setNames(c('.pred_setosa', '.pred_versicolor', '.pred_virginica'))

  parsnip_probs <- predict(cls_form, iris[ind, -5], type = "prob")
  expect_equal(as.data.frame(liquidSVM_probs), as.data.frame(parsnip_probs))

  parsnip_xy_probs <- predict(cls_xy_form, iris[ind, -5], type = "prob")
  expect_equal(as.data.frame(liquidSVM_probs), as.data.frame(parsnip_xy_probs))
})
