library(testthat)
library(parsnip)
library(rlang)
library(tibble)

# ------------------------------------------------------------------------------

context("RBF SVM")
source("helpers.R")

# ------------------------------------------------------------------------------

test_that('primary arguments', {
  basic <- svm_poly()
  basic_kernlab <- translate(basic %>% set_engine("kernlab"))

  expect_equal(
    object = basic_kernlab$method$fit$args,
    expected = list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      kernel = "polydot"
    )
  )

  degree <- svm_poly(degree = 2)
  degree_kernlab <- translate(degree %>% set_engine("kernlab"))
  degree_obj <- expr(list())
  degree_obj$degree <- new_empty_quosure(2)

  expect_equal(
    object = degree_kernlab$method$fit$args,
    expected = list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      kernel = "polydot",
      kpar = degree_obj
    )
  )

  degree_scale <- svm_poly(degree = 2, scale_factor = 1.2)
  degree_scale_kernlab <- translate(degree_scale %>% set_engine("kernlab"))
  degree_scale_obj <- expr(list())
  degree_scale_obj$degree <- new_empty_quosure(2)
  degree_scale_obj$scale <- new_empty_quosure(1.2)

  expect_equal(
    object = degree_scale_kernlab$method$fit$args,
    expected = list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      kernel = "polydot",
      kpar = degree_scale_obj
    )
  )

})

test_that('engine arguments', {

  kernlab_cv <- svm_poly() %>% set_engine("kernlab", cross = 10)

  expect_equal(
    object = translate(kernlab_cv, "kernlab")$method$fit$args,
    expected = list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      cross = new_empty_quosure(10),
      kernel = "polydot"
    )
  )

})


test_that('updating', {

  expr1     <- svm_poly()  %>% set_engine("kernlab", cross = 10)
  expr1_exp <- svm_poly(degree = 1) %>% set_engine("kernlab", cross = 10)

  expr2     <- svm_poly(degree = varying()) %>% set_engine("kernlab")
  expr2_exp <- svm_poly(degree = varying(), scale_factor = 1) %>% set_engine("kernlab")

  expr3     <- svm_poly(degree = 2, scale_factor = varying()) %>% set_engine("kernlab")
  expr3_exp <- svm_poly(degree = 3) %>% set_engine("kernlab")

  expect_equal(update(expr1, degree = 1), expr1_exp)
  expect_equal(update(expr2,  scale_factor = 1), expr2_exp)
  expect_equal(update(expr3, degree = 3, fresh = TRUE), expr3_exp)
})

test_that('bad input', {
  expect_error(svm_poly(mode = "reallyunknown"))
  expect_error(translate(svm_poly() %>% set_engine( NULL)))
})

# ------------------------------------------------------------------------------

reg_mod <-
  svm_poly(degree = 1, cost = 1/4) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

cls_mod <-
  svm_poly(degree = 2, cost = 1/8) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

ctrl <- fit_control(verbosity = 0, catch = FALSE)

# ------------------------------------------------------------------------------

test_that('svm poly regression', {

  skip_if_not_installed("kernlab")

  expect_error(
    fit_xy(
      reg_mod,
      control = ctrl,
      x = iris[,2:4],
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


test_that('svm poly regression prediction', {

  skip_if_not_installed("kernlab")

  reg_form <-
    fit(
      reg_mod,
      Sepal.Length ~ .,
      data = iris[, -5],
      control = ctrl
    )

  # kern_pred <-
  #   predict(reg_form$fit, iris[1:3, -c(1, 5)]) %>%
  #   as_tibble() %>%
  #   setNames(".pred")
  kern_pred <-
    structure(
      list(
        .pred = c(5.02154233477783, 4.71496213707127, 4.78370369917621)),
      row.names = c(NA,-3L),
      class = c("tbl_df", "tbl", "data.frame")
    )

  parsnip_pred <- predict(reg_form, iris[1:3, -c(1, 5)])
  expect_equal(as.data.frame(kern_pred), as.data.frame(parsnip_pred))


  reg_xy_form <-
    fit_xy(
      reg_mod,
      x = iris[, 2:4],
      y = iris$Sepal.Length,
      control = ctrl
    )
  expect_equal(reg_form$fit, reg_xy_form$fit)

  parsnip_xy_pred <- predict(reg_xy_form, iris[1:3, -c(1, 5)])
  expect_equal(as.data.frame(kern_pred), as.data.frame(parsnip_xy_pred))
})

# ------------------------------------------------------------------------------

test_that('svm poly classification', {

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


test_that('svm poly classification probabilities', {

  skip_if_not_installed("kernlab")

  ind <- c(1, 51, 101)

  set.seed(34562)
  cls_form <-
    fit(
      cls_mod,
      Species ~ .,
      data = iris,
      control = ctrl
    )

  # kern_class <-
  #   tibble(.pred_class = predict(cls_form$fit, iris[ind, -5]))

  kern_class <-
    structure(
      list(
        .pred_class =
          structure(1:3, .Label = c("setosa", "versicolor", "virginica"), class = "factor")),
      row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))

  parsnip_class <- predict(cls_form, iris[ind, -5])
  expect_equal(kern_class, parsnip_class)

  set.seed(34562)
  cls_xy_form <-
    fit_xy(
      cls_mod,
      x = iris[, 1:4],
      y = iris$Species,
      control = ctrl
    )
  expect_equal(cls_form$fit, cls_xy_form$fit)

  # kern_probs <-
  #   predict(cls_form$fit, iris[ind, -5], type = "probabilities") %>%
  #   as_tibble() %>%
  #   setNames(c('.pred_setosa', '.pred_versicolor', '.pred_virginica'))

  kern_probs <-
    structure(
      list(
        .pred_setosa = c(0.982990083267231, 0.0167077303224448, 0.00930879923686657),
        .pred_versicolor = c(0.00417116710624842, 0.946131931665357, 0.0015524073332013),
        .pred_virginica = c(0.0128387496265202, 0.0371603380121978, 0.989138793429932)),
      row.names = c(NA,-3L),
      class = c("tbl_df", "tbl", "data.frame"))

  parsnip_probs <- predict(cls_form, iris[ind, -5], type = "prob")
  expect_equal(as.data.frame(kern_probs), as.data.frame(parsnip_probs))

  parsnip_xy_probs <- predict(cls_xy_form, iris[ind, -5], type = "prob")
  expect_equal(as.data.frame(kern_probs), as.data.frame(parsnip_xy_probs))
})
