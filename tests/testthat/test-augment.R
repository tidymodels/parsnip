test_that('regression models', {
  x <- linear_reg() |> set_engine("lm")

  reg_form <- x |> fit(mpg ~ ., data = mtcars)
  reg_xy <- x |> fit_xy(mtcars[, -1], mtcars$mpg)

  expect_equal(
    colnames(augment(reg_form, head(mtcars))),
    c( ".pred", ".resid",
       "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
      "gear", "carb")
  )
  expect_equal(nrow(augment(reg_form, head(mtcars))), 6)
  expect_equal(
    colnames(augment(reg_form, head(mtcars[, -1]))),
    c(".pred",
    "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
      "gear", "carb")
  )
  expect_equal(nrow(augment(reg_form, head(mtcars[, -1]))), 6)

  expect_equal(
    colnames(augment(reg_xy, head(mtcars))),
    c(".pred",
      "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
      "gear", "carb")
  )
  expect_equal(nrow(augment(reg_xy, head(mtcars))), 6)
  expect_equal(
    colnames(augment(reg_xy, head(mtcars[, -1]))),
    c(".pred",
      "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
      "gear", "carb")
  )
  expect_equal(nrow(augment(reg_xy, head(mtcars[, -1]))), 6)

  expect_s3_class(augment(reg_form, head(mtcars)), "tbl_df")

  reg_form$spec$mode <- "depeche"

  expect_snapshot(
    error = TRUE,
    augment(reg_form, head(mtcars[, -1]))
  )

})



test_that('classification models', {
  skip_if_not_installed("modeldata")

  data(two_class_dat, package = "modeldata")
  x <- logistic_reg() |> set_engine("glm")

  cls_form <- x |> fit(Class ~ ., data = two_class_dat)
  cls_xy <- x |> fit_xy(two_class_dat[, -3], two_class_dat$Class)

  expect_equal(
    colnames(augment(cls_form, head(two_class_dat))),
    c(".pred_class", ".pred_Class1", ".pred_Class2", "A", "B", "Class")
  )
  expect_equal(nrow(augment(cls_form, head(two_class_dat))), 6)
  expect_equal(
    colnames(augment(cls_form, head(two_class_dat[, -3]))),
    c(".pred_class", ".pred_Class1", ".pred_Class2", "A", "B")
  )
  expect_equal(nrow(augment(cls_form, head(two_class_dat[, -3]))), 6)

  expect_equal(
    colnames(augment(cls_xy, head(two_class_dat))),
    c(".pred_class", ".pred_Class1", ".pred_Class2", "A", "B", "Class")
  )
  expect_equal(nrow(augment(cls_xy, head(two_class_dat))), 6)
  expect_equal(
    colnames(augment(cls_xy, head(two_class_dat[, -3]))),
    c(".pred_class", ".pred_Class1", ".pred_Class2", "A", "B")
  )
  expect_equal(nrow(augment(cls_xy, head(two_class_dat[, -3]))), 6)

})


test_that('augment for model without class probabilities', {
  skip_if_not_installed("LiblineaR")
  skip_if_not_installed("modeldata")

  data(two_class_dat, package = "modeldata")
  x <- svm_linear(mode = "classification") |> set_engine("LiblineaR")
  cls_form <- x |> fit(Class ~ ., data = two_class_dat)

  expect_equal(
    colnames(augment(cls_form, head(two_class_dat))),
    c(".pred_class", "A", "B", "Class")
  )
  expect_equal(nrow(augment(cls_form, head(two_class_dat))), 6)

})


test_that('quantile regression models', {
  probs_1 <- (1:5)/5

  expect_snapshot(
    linear_reg() |> set_mode("quantile regression", quantile_levels = probs_1)
  )
  expect_snapshot(
    linear_reg() |> set_mode("regression", quantile_levels = probs_1)
  )
})
