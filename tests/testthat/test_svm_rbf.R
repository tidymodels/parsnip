library(testthat)
library(parsnip)
library(rlang)

# ------------------------------------------------------------------------------

context("poly SVM")
source("helpers.R")

# ------------------------------------------------------------------------------

test_that('primary arguments', {
  basic <- svm_rbf()
  basic_kernlab <- translate(basic %>% set_engine("kernlab"))

  expect_equal(
    object = basic_kernlab$method$fit$args,
    expected = list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      kernel = "rbfdot"
    )
  )

  rbf_sigma <- svm_rbf(rbf_sigma = .2)
  rbf_sigma_kernlab <- translate(rbf_sigma %>% set_engine("kernlab"))
  rbf_sigma_obj <- expr(list())
  rbf_sigma_obj$sigma <- new_empty_quosure(.2)

  expect_equal(
    object = rbf_sigma_kernlab$method$fit$args,
    expected = list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      kernel = "rbfdot",
      kpar = rbf_sigma_obj
    )
  )

})

test_that('engine arguments', {

  kernlab_cv <- svm_rbf() %>% set_engine("kernlab", cross = 10)

  expect_equal(
    object = translate(kernlab_cv, "kernlab")$method$fit$args,
    expected = list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      cross = new_empty_quosure(10),
      kernel = "rbfdot"
    )
  )

})


test_that('updating', {

  expr1     <- svm_rbf()  %>% set_engine("kernlab", cross = 10)
  expr1_exp <- svm_rbf(rbf_sigma = .1) %>% set_engine("kernlab", cross = 10)

  expr3     <- svm_rbf(rbf_sigma = .2) %>% set_engine("kernlab")
  expr3_exp <- svm_rbf(rbf_sigma = .3) %>% set_engine("kernlab")

  expect_equal(update(expr1, rbf_sigma = .1), expr1_exp)
  expect_equal(update(expr3, rbf_sigma = .3, fresh = TRUE), expr3_exp)
})

test_that('bad input', {
  expect_error(svm_rbf(mode = "reallyunknown"))
  expect_error(translate(svm_rbf() %>% set_engine( NULL)))
})

# ------------------------------------------------------------------------------

reg_mod <-
  svm_rbf(rbf_sigma = .1, cost = 1/4) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

cls_mod <-
  svm_rbf(rbf_sigma = .1, cost = 1/8) %>%
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


test_that('svm rbf regression prediction', {

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
      list(.pred = c(5.02786147259765, 4.81715220026091, 4.86817852816449)),
      row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))

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
    structure(list(
      .pred_class = structure(
        c(1L, 3L, 3L),
        .Label = c("setosa", "versicolor", "virginica"), class = "factor")),
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
        .pred_setosa     = c(0.985403715135807, 0.0158818274678279, 0.00633995479908973),
        .pred_versicolor = c(0.00818691538722139, 0.359005663318986, 0.0173471664171275),
        .pred_virginica  = c(0.00640936947697121, 0.625112509213187, 0.976312878783783)),
      row.names = c(NA,-3L), class = c("tbl_df", "tbl", "data.frame"))

  parsnip_probs <- predict(cls_form, iris[ind, -5], type = "prob")
  expect_equal(as.data.frame(kern_probs), as.data.frame(parsnip_probs))

  parsnip_xy_probs <- predict(cls_xy_form, iris[ind, -5], type = "prob")
  expect_equal(as.data.frame(kern_probs), as.data.frame(parsnip_xy_probs))
})
