test_that('regression models', {
  x <- linear_reg() %>% set_engine("lm")

  reg_form <- x %>% fit(mpg ~ ., data = mtcars)
  reg_xy <- x %>% fit_xy(mtcars[, -1], mtcars$mpg)

  expect_equal(
    colnames(augment(reg_form, head(mtcars))),
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
      "gear", "carb", ".pred", ".resid")
  )
  expect_equal(nrow(augment(reg_form, head(mtcars))), 6)
  expect_equal(
    colnames(augment(reg_form, head(mtcars[, -1]))),
    c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
      "gear", "carb", ".pred")
  )
  expect_equal(nrow(augment(reg_form, head(mtcars[, -1]))), 6)

  expect_equal(
    colnames(augment(reg_xy, head(mtcars))),
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
      "gear", "carb", ".pred")
  )
  expect_equal(nrow(augment(reg_xy, head(mtcars))), 6)
  expect_equal(
    colnames(augment(reg_xy, head(mtcars[, -1]))),
    c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
      "gear", "carb", ".pred")
  )
  expect_equal(nrow(augment(reg_xy, head(mtcars[, -1]))), 6)

  expect_s3_class(augment(reg_form, head(mtcars)), "tbl_df")

  reg_form$spec$mode <- "depeche"

  expect_error(augment(reg_form, head(mtcars[, -1])), "Unknown mode: depeche")

})



test_that('classification models', {
  data(two_class_dat, package = "modeldata")
  x <- logistic_reg() %>% set_engine("glm")

  cls_form <- x %>% fit(Class ~ ., data = two_class_dat)
  cls_xy <- x %>% fit_xy(two_class_dat[, -3], two_class_dat$Class)

  expect_equal(
    colnames(augment(cls_form, head(two_class_dat))),
    c("A", "B", "Class", ".pred_class", ".pred_Class1", ".pred_Class2")
  )
  expect_equal(nrow(augment(cls_form, head(two_class_dat))), 6)
  expect_equal(
    colnames(augment(cls_form, head(two_class_dat[, -3]))),
    c("A", "B", ".pred_class", ".pred_Class1", ".pred_Class2")
  )
  expect_equal(nrow(augment(cls_form, head(two_class_dat[, -3]))), 6)

  expect_equal(
    colnames(augment(cls_xy, head(two_class_dat))),
    c("A", "B", "Class", ".pred_class", ".pred_Class1", ".pred_Class2")
  )
  expect_equal(nrow(augment(cls_xy, head(two_class_dat))), 6)
  expect_equal(
    colnames(augment(cls_xy, head(two_class_dat[, -3]))),
    c("A", "B", ".pred_class", ".pred_Class1", ".pred_Class2")
  )
  expect_equal(nrow(augment(cls_xy, head(two_class_dat[, -3]))), 6)

})


test_that('augment for model without class probabilities', {
  skip_if_not_installed("LiblineaR")

  data(two_class_dat, package = "modeldata")
  x <- svm_linear(mode = "classification") %>% set_engine("LiblineaR")
  cls_form <- x %>% fit(Class ~ ., data = two_class_dat)

  expect_equal(
    colnames(augment(cls_form, head(two_class_dat))),
    c("A", "B", "Class", ".pred_class")
  )
  expect_equal(nrow(augment(cls_form, head(two_class_dat))), 6)

})
