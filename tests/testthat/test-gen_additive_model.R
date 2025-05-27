skip_if_not_installed("modeldata")

test_that('regression', {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("modeldata")

  reg_mod <-
    gen_additive_mod(select_features = TRUE) |>
    set_engine("mgcv") |>
    set_mode("regression")

  expect_no_condition(
    f_res <- fit(
      reg_mod,
      mpg ~ s(disp) + wt + gear,
      data = mtcars
    )
  )
  expect_snapshot(
    error = TRUE,
    xy_res <- fit_xy(
      reg_mod,
      x = mtcars[, 1:5],
      y = mtcars$mpg,
      control = ctrl
    )
  )
  mgcv_mod <- mgcv::gam(mpg ~ s(disp) + wt + gear, data = mtcars, select = TRUE)
  expect_equal(coef(mgcv_mod), coef(extract_fit_engine(f_res)))

  f_pred <- predict(f_res, head(mtcars))
  mgcv_pred <- predict(mgcv_mod, head(mtcars), type = "response")
  expect_equal(names(f_pred), ".pred")
  expect_equal(f_pred[[".pred"]], mgcv_pred, ignore_attr = TRUE)

  f_ci <- predict(f_res, head(mtcars), type = "conf_int", std_error = TRUE)
  mgcv_ci <- predict(mgcv_mod, head(mtcars), type = "link", se.fit = TRUE)
  expect_equal(f_ci[[".std_error"]], mgcv_ci$se.fit)
  lower <-
    mgcv_ci$fit - qt(0.025, df = mgcv_mod$df.residual, lower.tail = FALSE) * mgcv_ci$se.fit
  expect_equal(f_ci[[".pred_lower"]], lower)

})

# ------------------------------------------------------------------------------

test_that('classification', {
  skip_if_not_installed("mgcv")

  cls_mod <-
    gen_additive_mod(adjust_deg_free = 1.5) |>
    set_engine("mgcv") |>
    set_mode("classification")

  expect_no_condition(
    f_res <- fit(
      cls_mod,
      Class ~ s(A, k = 10) + B,
      data = two_class_dat
    )
  )
  expect_snapshot(
    error = TRUE,
    xy_res <- fit_xy(
      cls_mod,
      x = two_class_dat[, 2:3],
      y = two_class_dat$Class,
      control = ctrl
    )
  )
  mgcv_mod <-
    mgcv::gam(Class ~ s(A, k = 10) + B,
              data = two_class_dat,
              gamma = 1.5,
              family = binomial)
  expect_equal(coef(mgcv_mod), coef(extract_fit_engine(f_res)))

  f_pred <- predict(f_res, head(two_class_dat), type = "prob")
  mgcv_pred <- predict(mgcv_mod, head(two_class_dat), type = "response")
  expect_equal(names(f_pred), c(".pred_Class1", ".pred_Class2"))
  expect_equal(f_pred[[".pred_Class2"]], mgcv_pred, ignore_attr = TRUE)
  expect_equal(class(f_pred[[".pred_Class1"]]), "numeric")
  expect_equal(class(f_pred[[".pred_Class2"]]), "numeric")

  f_cls <- predict(f_res, head(two_class_dat), type = "class")
  expect_true(all(f_cls$.pred_class[mgcv_pred < 0.5] == "Class1"))

  f_ci <- predict(f_res, head(two_class_dat), type = "conf_int", std_error = TRUE)
  mgcv_ci <- predict(mgcv_mod, head(two_class_dat), type = "link", se.fit = TRUE)
  expect_equal(f_ci[[".std_error"]], mgcv_ci$se.fit)
  lower <-
    mgcv_ci$fit - qt(0.025, df = mgcv_mod$df.residual, lower.tail = FALSE) * mgcv_ci$se.fit
  lower <- binomial()$linkinv(lower)
  expect_equal(f_ci[[".pred_lower_Class2"]], lower)

})

test_that("check_args() works", {
  # Here for completeness, no checking is done
  expect_true(TRUE)
})
