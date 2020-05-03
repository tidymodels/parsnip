library(testthat)
library(parsnip)
library(rlang)

# ------------------------------------------------------------------------------

num_pred <- c("Sepal.Width", "Petal.Width", "Petal.Length")
iris_bad_form <- as.formula(Species ~ term)
iris_basic <- nearest_neighbor(neighbors = 8) %>% set_engine("FNN")
clf_ind <- c(1, 51, 101, 25)

ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

# ------------------------------------------------------------------------------

test_that('FNN execution', {
  
  skip_if_not_installed("FNN")
  library(FNN)
  
  # continuous
  # expect no error
  expect_error(
    fit_xy(
      iris_basic,
      control = ctrl,
      x = iris[, num_pred],
      y = iris$Sepal.Length
    ),
    regexp = NA
  )
  
  # nominal
  # expect no error
  expect_error(
    fit_xy(
      iris_basic,
      control = ctrl,
      x = iris[, c("Sepal.Length", "Petal.Width")],
      y = iris$Species
    ),
    regexp = NA
  )
  
  expect_error(
    fit(
      iris_basic,
      iris_bad_form,
      data = iris,
      control = ctrl
    )
  )
  
})

test_that('FNN prediction', {
  
  skip_if_not_installed("FNN")
  
  # regression
  reg_xy <- fit_xy(
    iris_basic,
    control = ctrl,
    x = iris[, num_pred],
    y = iris$Sepal.Length
  )
  
  reg_xy_pred <- predict(reg_xy, iris[1:5, num_pred])
  
  reg_fnn_pred <- FNN::knn.reg(
    train = iris[, num_pred],
    y = iris$Sepal.Length,
    k = 8,
    test = iris[1:5, num_pred]
  )[["pred"]]
  
  expect_equal(reg_fnn_pred, reg_xy_pred[[1]])
  
  # nominal
  cls_xy <- fit_xy(
    iris_basic %>% set_mode("classification"),
    control = ctrl,
    x = iris[, c("Sepal.Length", "Petal.Width")],
    y = iris$Species
  )
  
  cls_xy_pred <- predict(cls_xy, iris[1:5, c("Sepal.Length", "Petal.Width")], )
  
  cls_fnn_pred <- FNN::knn(
    train = iris[, c("Sepal.Length", "Petal.Width")],
    cl = iris$Species,
    k = 8,
    test = iris[1:5, c("Sepal.Length", "Petal.Width")]
  )
  
  lvl <- levels(iris$Species)
  attributes(cls_fnn_pred) <- NULL
  cls_fnn_pred <- factor(lvl[cls_fnn_pred], levels = lvl)
  
  cls_fnn_pred <- structure(
    list(.pred_class = cls_fnn_pred),
    row.names = c(NA, -5L),
    class = c("tbl_df", "tbl", "data.frame"))
  
  expect_equal(cls_fnn_pred, cls_xy_pred)
  
  # continuous - formula interface
  reg_form <- fit(
    iris_basic %>% set_mode("regression"),
    Sepal.Length ~ Sepal.Width + Petal.Width + Petal.Length,
    data = iris,
    control = ctrl
  )
  
  reg_form_pred <- predict(
    reg_form,
    new_data = iris[1:5,]
  )
  
  expect_equal(reg_form_pred[[1]], reg_fnn_pred)
})