library(testthat)
library(parsnip)
library(rlang)
library(tibble)

source(test_path("helper-objects.R"))
hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

test_that('primary arguments', {
  basic <- svm_rbf(mode = "classification")
  basic_liquidSVM <- translate(basic %>% set_engine("liquidSVM"))

  expect_equal(
    object = basic_liquidSVM$method$fit$args,
    expected = list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      folds = 1,
      threads = 0
    )
  )

  rbf_sigma <-
    svm_rbf(mode = "classification", rbf_sigma = .2) %>%
    set_engine("liquidSVM")
  rbf_sigma_liquidSVM <- translate(rbf_sigma)

  expect_equal(
    object = rbf_sigma_liquidSVM$method$fit$args,
    expected = list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      gammas = quo(.2),
      folds = 1,
      threads = 0
    )
  )

})

test_that('engine arguments', {

  liquidSVM_scale <-
    svm_rbf() %>%
    set_mode("classification") %>%
    set_engine("liquidSVM", scale = FALSE, predict.prob = TRUE, threads = 2, gpus = 1)

  expect_equal(
    object = translate(liquidSVM_scale, "liquidSVM")$method$fit$args,
    expected = list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      scale = new_quosure(FALSE, env = empty_env()),
      predict.prob = new_quosure(TRUE, env = empty_env()),
      threads = new_quosure(2, env = empty_env()),
      gpus = new_quosure(1, env = empty_env()),
      folds = 1
    )
  )

})


test_that('updating', {

  expr1 <- svm_rbf()  %>% set_engine("liquidSVM", scale = TRUE)
  expr1_exp <- svm_rbf(rbf_sigma = .1) %>% set_engine("liquidSVM", scale = TRUE)

  expr3 <- svm_rbf(rbf_sigma = .2) %>% set_engine("liquidSVM")
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
  set_engine("liquidSVM", random_seed = 1234, folds = 1, max_gamma=500) %>%
  set_mode("regression")

cls_mod <-
  svm_rbf(rbf_sigma = .1, cost = 0.125) %>%
  set_engine("liquidSVM", random_seed = 1234, folds = 1) %>%
  set_mode("classification")

ctrl <- fit_control(verbosity = 0, catch = FALSE)

# ------------------------------------------------------------------------------

test_that('svm rbf regression', {

  skip_if_not_installed("liquidSVM")

  expect_warning(
    expect_error(
      fit_xy(
        reg_mod,
        control = ctrl,
        x = hpc[, 2:4],
        y = hpc$compounds
      ),
      regexp = NA
    )
  )

  expect_warning(
    expect_error(
      fit(
        reg_mod,
        compounds ~ .,
        data = hpc[, -5],
        control = ctrl
      ),
      regexp = NA
    )
  )

})


test_that('svm rbf regression prediction', {

  skip_if_not_installed("liquidSVM")

  expect_warning(
    reg_form <-
      fit(
        object = reg_mod,
        formula = compounds ~ .,
        data = hpc[, -5],
        control = ctrl
      )
  )

  expect_warning(
    reg_xy_form <-
      fit_xy(
        object = reg_mod,
        x = hpc[, 2:4],
        y = hpc$compounds,
        control = ctrl
      )
  )
  expect_equal(reg_form$spec, reg_xy_form$spec)

  expect_warning(
    liquidSVM_form <-
      liquidSVM::svm(
        x = compounds ~ .,
        y = hpc[, -5],
        gammas = .1,
        lambdas = 0.25,
        folds = 1,
        random_seed = 1234
      )
  )

  expect_warning(
    liquidSVM_xy_form <-
      liquidSVM::svm(
        x = hpc[, 2:4],
        y = hpc$compounds,
        gammas = .1,
        lambdas = 0.25,
        folds = 1,
        random_seed = 1234
      )
  )

  # check coeffs for liquidSVM formula and liquidSVM xy fit interfaces
  expect_equal(liquidSVM::getSolution(liquidSVM_form)[c("coeff", "sv")],
               liquidSVM::getSolution(liquidSVM_xy_form)[c("coeff", "sv")])

  # check predictions for liquidSVM formula and liquidSVM xy interfaces
  liquidSVM_form_preds <- predict(liquidSVM_form, hpc[1:3, 2:4])
  liquidSVM_form_xy_preds <- predict(liquidSVM_xy_form, hpc[1:3, 2:4])
  expect_equal(liquidSVM_form_preds, liquidSVM_form_xy_preds)

  # check predictions for parsnip formula and liquidSVM formula interfaces
  liquidSVM_pred <-
    structure(
      list(.pred = liquidSVM_form_preds),
      row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))

  parsnip_pred <- predict(reg_form, hpc[1:3, 2:4])
  expect_equal(as.data.frame(liquidSVM_pred), as.data.frame(parsnip_pred))

  # check that coeffs are equal for formula methods called via parsnip and liquidSVM
  expect_equal(liquidSVM::getSolution(reg_form$fit)[c("coeff", "sv")],
               liquidSVM::getSolution(liquidSVM_form)[c("coeff", "sv")])

  # check coeffs are equivalent for parsnip fit_xy and parsnip formula methods
  expect_equal(liquidSVM::getSolution(reg_form$fit)[c("coeff", "sv")],
               liquidSVM::getSolution(reg_xy_form$fit)[c("coeff", "sv")])

  # check predictions are equal for parsnip xy and liquidSVM xy methods
  parsnip_xy_pred <- predict(reg_xy_form, hpc[1:3, -c(1, 5)])
  expect_equal(as.data.frame(liquidSVM_pred), as.data.frame(parsnip_xy_pred))
})

