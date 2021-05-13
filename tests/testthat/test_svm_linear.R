library(testthat)
library(parsnip)
library(rlang)

# ------------------------------------------------------------------------------

context("linear SVM")
source(test_path("helpers.R"))
source(test_path("helper-objects.R"))
hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

test_that('primary arguments', {
  basic <- svm_linear(mode = "regression")
  basic_LiblineaR <- translate(basic %>% set_engine("LiblineaR"))
  basic_kernlab <- translate(basic %>% set_engine("kernlab"))

  expect_equal(
    object = basic_LiblineaR$method$fit$args,
    expected = list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      wi = expr(missing_arg()),
      type = 11,
      svr_eps = 0.1
    )
  )

  expect_equal(
    object = basic_kernlab$method$fit$args,
    expected = list(
      x = expr(missing_arg()),
      data = expr(missing_arg()),
      kernel = "vanilladot"
    )
  )

})

test_that('engine arguments', {

  LiblineaR_type <- svm_linear(mode = "regression") %>% set_engine("LiblineaR", type = 12)
  kernlab_cv <- svm_linear(mode = "regression") %>% set_engine("kernlab", cross = 10)

  expect_equal(
    object = translate(LiblineaR_type, "LiblineaR")$method$fit$args,
    expected = list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      wi = expr(missing_arg()),
      type = new_empty_quosure(12),
      svr_eps = 0.1
    )
  )

  expect_equal(
    object = translate(kernlab_cv, "kernlab")$method$fit$args,
    expected = list(
      x = expr(missing_arg()),
      data = expr(missing_arg()),
      cross = new_empty_quosure(10),
      kernel = "vanilladot"
    )
  )

})


test_that('updating', {

  expr1     <- svm_linear(mode = "regression")  %>% set_engine("LiblineaR", type = 12)
  expr1_exp <- svm_linear(mode = "regression", cost = 3) %>% set_engine("LiblineaR", type = 12)
  expr2     <- svm_linear(mode = "regression") %>% set_engine("LiblineaR", type = varying())
  expr2_exp <- svm_linear(mode = "regression") %>% set_engine("LiblineaR", type = 13)
  expr3     <- svm_linear(mode = "regression", cost = 2) %>% set_engine("LiblineaR")
  expr3_exp <- svm_linear(mode = "regression", cost = 5) %>% set_engine("LiblineaR")
  expr4     <- svm_linear(mode = "regression")  %>% set_engine("kernlab", cross = 10)
  expr4_exp <- svm_linear(mode = "regression", cost = 2) %>% set_engine("kernlab", cross = 10)
  expr5     <- svm_linear(mode = "regression") %>% set_engine("kernlab", cross = varying())
  expr5_exp <- svm_linear(mode = "regression") %>% set_engine("kernlab", cross = 10)


  expect_equal(update(expr1, cost = 3), expr1_exp)
  expect_equal(update(expr2, type = 13), expr2_exp)
  expect_equal(update(expr3, cost = 5, fresh = TRUE), expr3_exp)

  param_tibb <- tibble::tibble(margin = 0.05, cost = 10)
  param_list <- as.list(param_tibb)

  expr1_updated <- update(expr1, param_tibb)
  expect_equal(expr1_updated$args$margin, 0.05)
  expect_equal(expr1_updated$args$cost, 10)
  expect_equal(expr1_updated$eng_args$type, rlang::quo(12))

  expr1_updated_lst <- update(expr1, param_list)
  expect_equal(expr1_updated_lst$args$margin, 0.05)
  expect_equal(expr1_updated_lst$args$cost, 10)
  expect_equal(expr1_updated_lst$eng_args$type, rlang::quo(12))

  expect_equal(update(expr4, cost = 2), expr4_exp)
  expect_equal(update(expr5, cross = 10), expr5_exp)

})

test_that('bad input', {
  expect_warning(translate(svm_linear(mode = "regression") %>% set_engine( NULL)))
  expect_error(svm_linear(mode = "reallyunknown"))
  expect_error(translate(svm_linear(mode = "regression") %>% set_engine("LiblineaR", type = 3)))
  expect_error(translate(svm_linear(mode = "classification") %>% set_engine("LiblineaR", type = 11)))
})

# ------------------------------------------------------------------------------

reg_mod <-
  svm_linear(mode = "regression", cost = 1/4) %>%
  set_engine("LiblineaR") %>%
  set_mode("regression")

cls_mod <-
  svm_linear(mode = "classification", cost = 1/8) %>%
  set_engine("LiblineaR") %>%
  set_mode("classification")

ctrl <- control_parsnip(verbosity = 0, catch = FALSE)

# ------------------------------------------------------------------------------

test_that('linear svm regression: LiblineaR', {

  skip_if_not_installed("LiblineaR")

  expect_error(
    res <- fit_xy(
      reg_mod,
      control = ctrl,
      x = hpc[,2:4],
      y = hpc$input_fields
    ),
    regexp = NA
  )
  expect_false(has_multi_predict(res))
  expect_equal(multi_predict_args(res), NA_character_)
  expect_output(print(res), "parsnip model object")

  expect_error(
    tidy_res <- tidy(res),
    NA
  )
  expect_s3_class(tidy_res, "tbl_df")
  expect_equal(colnames(tidy_res), c("term", "estimate"))

  expect_error(
    fit(
      reg_mod,
      input_fields ~ .,
      data = hpc[, -5],
      control = ctrl
    ),
    regexp = NA
  )

})


test_that('linear svm regression prediction: LiblineaR', {

  skip_if_not_installed("LiblineaR")

  hpc_no_m <- hpc[-c(84, 85, 86, 87, 88, 109, 128),] %>%
    droplevels()

  ind <- c(2, 1, 143)

  reg_form <-
    fit(
      reg_mod,
      input_fields ~ .,
      data = hpc[, -5],
      control = ctrl
    )

  liblinear_pred <-
    structure(
      list(.pred = c(85.13979, 576.16232, 1886.10132)),
      row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))

  parsnip_pred <- predict(reg_form, hpc[ind, -c(2, 5)])
  expect_equal(as.data.frame(liblinear_pred),
               as.data.frame(parsnip_pred),
               tolerance = .0001)


  reg_xy_form <-
    fit_xy(
      reg_mod,
      x = hpc[, c(1, 3, 4)],
      y = hpc$input_fields,
      control = ctrl
    )
  expect_equal(reg_form$fit$W, reg_xy_form$fit$W)

  parsnip_xy_pred <- predict(reg_xy_form, hpc[ind, -c(2, 5)])
  expect_equal(as.data.frame(liblinear_pred),
               as.data.frame(parsnip_xy_pred),
               tolerance = .0001)
})

# ------------------------------------------------------------------------------

test_that('linear svm classification: LiblineaR', {

  skip_if_not_installed("LiblineaR")

  hpc_no_m <- hpc[-c(84, 85, 86, 87, 88, 109, 128),] %>%
    droplevels()

  ind <- c(2, 1, 143)

  expect_error(
    fit_xy(
      cls_mod,
      control = ctrl,
      x = hpc_no_m[, -5],
      y = hpc_no_m$class
    ),
    regexp = NA
  )

  expect_error(
    fit(
      cls_mod,
      class ~ .,
      data = hpc_no_m,
      control = ctrl
    ),
    regexp = NA
  )

})


test_that('linear svm classification prediction: LiblineaR', {

  skip_if_not_installed("LiblineaR")

  hpc_no_m <- hpc[-c(84, 85, 86, 87, 88, 109, 128),] %>%
    droplevels()

  ind <- c(4, 55, 143)

  set.seed(34562)
  cls_form <-
    fit(
      cls_mod,
      class ~ .,
      data = hpc_no_m,
      control = ctrl
    )

  liblinear_class <-
    structure(list(
      .pred_class = structure(
        c(1L, 1L, 2L),
        .Label = c("VF", "F", "L"), class = "factor")),
      row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))

  parsnip_class <- predict(cls_form, hpc_no_m[ind, -5])
  expect_equal(liblinear_class, parsnip_class)

  set.seed(34562)
  cls_xy_form <-
    fit_xy(
      cls_mod,
      x = hpc_no_m[, 1:4],
      y = hpc_no_m$class,
      control = ctrl
    )
  expect_equal(cls_form$fit$W, cls_xy_form$fit$W)

  expect_error(
    predict(cls_form, hpc_no_m[ind, -5], type = "prob"),
    "No prob prediction method available for this model"
  )

  expect_error(
    predict(cls_xy_form, hpc_no_m[ind, -5], type = "prob"),
    "No prob prediction method available for this model"
  )

})

# ------------------------------------------------------------------------------

reg_mod <-
  svm_linear(mode = "regression", cost = 1/4) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

cls_mod <-
  svm_linear(mode = "classification", cost = 1/8) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

ctrl <- control_parsnip(verbosity = 0, catch = FALSE)

# ------------------------------------------------------------------------------

test_that('linear svm regression: kernlab', {

  skip_if_not_installed("kernlab")

  expect_error(
    res <- fit_xy(
      reg_mod,
      control = ctrl,
      x = hpc[,2:4],
      y = hpc$input_fields
    ),
    regexp = NA
  )
  expect_false(has_multi_predict(res))
  expect_equal(multi_predict_args(res), NA_character_)
  expect_output(print(res), "parsnip model object")

  expect_error(
    fit(
      reg_mod,
      input_fields ~ .,
      data = hpc[, -5],
      control = ctrl
    ),
    regexp = NA
  )

})


test_that('linear svm regression prediction: kernlab', {

  skip_if_not_installed("kernlab")

  hpc_no_m <- hpc[-c(84, 85, 86, 87, 88, 109, 128),] %>%
    droplevels()

  ind <- c(2, 1, 143)

  reg_form <-
    fit(
      reg_mod,
      input_fields ~ .,
      data = hpc[, -5],
      control = ctrl
    )

  kernlab_pred <-
    structure(
      list(.pred = c(129.9097, 376.1049, 1032.8989)),
      row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))

  parsnip_pred <- predict(reg_form, hpc[ind, -c(2, 5)])
  expect_equal(as.data.frame(kernlab_pred),
               as.data.frame(parsnip_pred),
               tolerance = .0001)


  reg_xy_form <-
    fit_xy(
      reg_mod,
      x = hpc[, c(1, 3, 4)],
      y = hpc$input_fields,
      control = ctrl
    )
  expect_equal(reg_form$fit@alphaindex, reg_xy_form$fit@alphaindex)

  parsnip_xy_pred <- predict(reg_xy_form, hpc[ind, -c(2, 5)])
  expect_equal(as.data.frame(kernlab_pred),
               as.data.frame(parsnip_xy_pred),
               tolerance = .0001)
})

# ------------------------------------------------------------------------------

test_that('linear svm classification: kernlab', {

  skip_if_not_installed("kernlab")

  hpc_no_m <- hpc[-c(84, 85, 86, 87, 88, 109, 128),] %>%
    droplevels()

  ind <- c(2, 1, 143)

  expect_error(
    fit_xy(
      cls_mod,
      control = ctrl,
      x = hpc_no_m[, -5],
      y = hpc_no_m$class
    ),
    regexp = NA
  )

  expect_error(
    fit(
      cls_mod,
      class ~ .,
      data = hpc_no_m,
      control = ctrl
    ),
    regexp = NA
  )

})


test_that('linear svm classification prediction: kernlab', {

  skip_if_not_installed("kernlab")

  hpc_no_m <- hpc[-c(84, 85, 86, 87, 88, 109, 128),] %>%
    droplevels()

  ind <- c(4, 55, 143)

  set.seed(34562)
  cls_form <-
    fit(
      cls_mod,
      class ~ .,
      data = hpc_no_m,
      control = ctrl
    )

  kernlab_class <-
    structure(list(
      .pred_class = structure(
        c(1L, 1L, 3L),
        .Label = c("VF", "F", "L"), class = "factor")),
      row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))

  parsnip_class <- predict(cls_form, hpc_no_m[ind, -5])
  expect_equal(kernlab_class, parsnip_class)

  set.seed(34562)
  cls_xy_form <-
    fit_xy(
      cls_mod,
      x = hpc_no_m[, 1:4],
      y = hpc_no_m$class,
      control = ctrl
    )
  expect_equal(cls_form$fit@alphaindex, cls_xy_form$fit@alphaindex)

  library(kernlab)
  kern_probs <-
    kernlab::predict(cls_form$fit, hpc_no_m[ind, -5], type = "probabilities") %>%
    as_tibble() %>%
    setNames(c('.pred_VF', '.pred_F', '.pred_L'))

  parsnip_probs <- predict(cls_form, hpc_no_m[ind, -5], type = "prob")
  expect_equal(as.data.frame(kern_probs), as.data.frame(parsnip_probs))

  parsnip_xy_probs <- predict(cls_xy_form, hpc_no_m[ind, -5], type = "prob")
  expect_equal(as.data.frame(kern_probs), as.data.frame(parsnip_xy_probs))

})



