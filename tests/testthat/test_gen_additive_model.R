library(testthat)
library(parsnip)
library(rlang)
library(tibble)
library(mgcv)

data(two_class_dat, package = "modeldata")

# ------------------------------------------------------------------------------

context("generalized additive models")

# ------------------------------------------------------------------------------


test_that('regression', {
  skip_if_not_installed("mgcv")

  reg_mod <-
    gen_additive_mod(select_features = TRUE) %>%
    set_engine("mgcv") %>%
    set_mode("regression")

  expect_error(
    f_res <- fit(
      reg_mod,
      mpg ~ s(disp) + wt + gear,
      data = mtcars
    ),
    regexp = NA
  )
  expect_error(
    xy_res <- fit_xy(
      reg_mod,
      x = mtcars[, 1:5],
      y = mtcars$mpg,
      control = ctrl
    ),
    regexp = "must be used with GAM models"
  )
  mgcv_mod <- mgcv::gam(mpg ~ s(disp) + wt + gear, data = mtcars, select = TRUE)
  expect_equal(coef(mgcv_mod), coef(f_res$fit))

  f_pred <- predict(f_res, head(mtcars))
  mgcv_pred <- predict(mgcv_mod, head(mtcars), type = "response")
  expect_equal(names(f_pred), ".pred")
  expect_equivalent(f_pred[[".pred"]], unname(mgcv_pred))

  f_ci <- predict(f_res, head(mtcars), type = "conf_int", std_error = TRUE)
  mgcv_ci <- predict(mgcv_mod, head(mtcars), type = "link", se.fit = TRUE)
  expect_equivalent(f_ci[[".std_error"]], unname(mgcv_ci$se.fit))
  lower <-
    mgcv_ci$fit - qt(0.025, df = mgcv_mod$df.residual, lower.tail = FALSE) * mgcv_ci$se.fit
  expect_equivalent(f_ci[[".pred_lower"]], unname(lower))

})

# ------------------------------------------------------------------------------

test_that('classification', {
  skip_if_not_installed("mgcv")

  cls_mod <-
    gen_additive_mod(adjust_deg_free = 1.5) %>%
    set_engine("mgcv") %>%
    set_mode("classification")

  expect_error(
    f_res <- fit(
      cls_mod,
      Class ~ s(A, k = 10) + B,
      data = two_class_dat
    ),
    regexp = NA
  )
  expect_error(
    xy_res <- fit_xy(
      cls_mod,
      x = two_class_dat[, 2:3],
      y = two_class_dat$Class,
      control = ctrl
    ),
    regexp = "must be used with GAM models"
  )
  mgcv_mod <-
    mgcv::gam(Class ~ s(A, k = 10) + B,
              data = two_class_dat,
              gamma = 1.5,
              family = binomial)
  expect_equal(coef(mgcv_mod), coef(f_res$fit))

  f_pred <- predict(f_res, head(two_class_dat), type = "prob")
  mgcv_pred <- predict(mgcv_mod, head(two_class_dat), type = "response")
  expect_equal(names(f_pred), c(".pred_Class1", ".pred_Class2"))
  expect_equivalent(f_pred[[".pred_Class2"]], unname(mgcv_pred))

  f_ci <- predict(f_res, head(two_class_dat), type = "conf_int", std_error = TRUE)
  mgcv_ci <- predict(mgcv_mod, head(two_class_dat), type = "link", se.fit = TRUE)
  expect_equivalent(f_ci[[".std_error"]], unname(mgcv_ci$se.fit))
  lower <-
    mgcv_ci$fit - qt(0.025, df = mgcv_mod$df.residual, lower.tail = FALSE) * mgcv_ci$se.fit
  lower <- binomial()$linkinv(lower)
  expect_equivalent(f_ci[[".pred_lower_Class2"]], unname(lower))

})
