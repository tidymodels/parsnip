library(testthat)
library(parsnip)
library(rlang)

# ------------------------------------------------------------------------------

context("nearest neighbor execution with kknn")

num_pred <- c("Sepal.Width", "Petal.Width", "Petal.Length")
iris_bad_form <- as.formula(Species ~ term)
iris_basic <- nearest_neighbor(mode = "classification",
                               neighbors = 8,
                               weight_func = "triangular") %>%
  set_engine("kknn")

ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

# ------------------------------------------------------------------------------

test_that('kknn execution', {

  skip_if_not_installed("kknn")
  library(kknn)

  # continuous
  # expect no error
  expect_error(
    fit_xy(
      iris_basic,
      control = ctrl,
      x = iris[, num_pred],
      y = iris$Sepal.Length
    ),
    regexp = "outcome should be a factor"
  )

  # nominal
  # expect no error
  expect_error(
    res <- fit_xy(
      iris_basic,
      control = ctrl,
      x = iris[, c("Sepal.Length", "Petal.Width")],
      y = iris$Species
    ),
    regexp = NA
  )

  expect_true(has_multi_predict(res))
  expect_equal(multi_predict_args(res), "neighbors")

  expect_error(
    fit(
      iris_basic,
      iris_bad_form,
      data = iris,

      control = ctrl
    )
  )

})

test_that('kknn prediction', {

  skip_if_not_installed("kknn")
  library(kknn)

  # continuous
  res_xy <- fit_xy(
    iris_basic,
    control = ctrl,
    x = iris[, num_pred],
    y = iris$Species
  )

  uni_pred <- predict(
    res_xy$fit,
    newdata = iris[1:5, num_pred]
  )

  expect_equal(tibble(.pred_class = uni_pred), predict(res_xy, iris[1:5, num_pred]))

  # nominal
  res_xy_nom <- fit_xy(
    iris_basic %>% set_mode("classification"),
    control = ctrl,
    x = iris[, c("Sepal.Length", "Petal.Width")],
    y = iris$Species
  )

  uni_pred_nom <- predict(
    res_xy_nom$fit,
    newdata = iris[1:5, c("Sepal.Length", "Petal.Width")]
  )

  expect_equal(
    uni_pred_nom,
    predict(res_xy_nom, iris[1:5, c("Sepal.Length", "Petal.Width")], type = "class")$.pred_class
  )

  # continuous - formula interface
  res_form <- fit(
    iris_basic %>% set_mode("regression"),
    Sepal.Length ~ log(Sepal.Width) + Species,
    data = iris,
    control = ctrl
  )

  form_pred <- predict(
    res_form$fit,
    newdata = iris[1:5,]
  )

  expect_equal(form_pred, predict(res_form, iris[1:5, c("Sepal.Width", "Species")])$.pred)
})


test_that('kknn multi-predict', {

  skip_if_not_installed("kknn")
  library(kknn)

  iris_te <- c(1:2, 50:51, 100:101)
  k_vals <- 1:10

  res_xy <- fit_xy(
    nearest_neighbor(mode = "classification", neighbors = 3) %>%
      set_engine("kknn"),
    control = ctrl,
    x = iris[-iris_te, num_pred],
    y = iris$Species[-iris_te]
  )

  pred_multi <- multi_predict(res_xy, iris[iris_te, num_pred], neighbors = k_vals)
  expect_equal(pred_multi %>% unnest() %>% nrow(), length(iris_te) * length(k_vals))
  expect_equal(pred_multi %>% nrow(), length(iris_te))

  pred_uni <- predict(res_xy, iris[iris_te, num_pred])
  pred_uni_obs <-
    pred_multi %>%
    mutate(.rows = row_number()) %>%
    unnest() %>%
    dplyr::filter(neighbors == 3) %>%
    arrange(.rows) %>%
    dplyr::select(.pred_class)
  expect_equal(pred_uni, pred_uni_obs)


  prob_multi <- multi_predict(res_xy, iris[iris_te, num_pred],
                              neighbors = k_vals, type = "prob")
  expect_equal(prob_multi %>% unnest() %>% nrow(), length(iris_te) * length(k_vals))
  expect_equal(prob_multi %>% nrow(), length(iris_te))

  prob_uni <- predict(res_xy, iris[iris_te, num_pred], type = "prob")
  prob_uni_obs <-
    prob_multi %>%
    mutate(.rows = row_number()) %>%
    unnest() %>%
    dplyr::filter(neighbors == 3) %>%
    arrange(.rows) %>%
    dplyr::select(!!names(prob_uni))
  expect_equal(prob_uni, prob_uni_obs)

  # ----------------------------------------------------------------------------
  # regression

  cars_te <- 1:5
  k_vals <- 1:10

  res_xy <- fit(
    nearest_neighbor(mode = "regression", neighbors = 3) %>%
      set_engine("kknn"),
    control = ctrl,
    mpg ~ ., data = mtcars[-cars_te, ]
  )

  pred_multi <- multi_predict(res_xy, mtcars[cars_te, -1], neighbors = k_vals)
  expect_equal(pred_multi %>% unnest() %>% nrow(), length(cars_te) * length(k_vals))
  expect_equal(pred_multi %>% nrow(), length(cars_te))

  pred_uni <- predict(res_xy, mtcars[cars_te, -1])
  pred_uni_obs <-
    pred_multi %>%
    mutate(.rows = row_number()) %>%
    unnest() %>%
    dplyr::filter(neighbors == 3) %>%
    arrange(.rows) %>%
    dplyr::select(.pred)
  expect_equal(pred_uni, pred_uni_obs)
})
