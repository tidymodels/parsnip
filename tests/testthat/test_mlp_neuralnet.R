library(testthat)
library(parsnip)
library(rlang)
library(tibble)
library(magrittr)

iris_df <- iris
iris_df[, 1:4] <- scale(iris_df[, 1:4])

# neuralnet execution on non-numeric data --------------------------------------
test_that("neuralnet execution on non-numeric data", {

  skip_if_not_installed("neuralnet")

  iris_neuralnet <-
    mlp(mode = "regression", hidden_units = 5, epochs = 5) %>%
    set_engine("neuralnet")

  # test for prediction on non-numeric data
  expect_error(
    fit(
      object = iris_neuralnet,
      formula = Petal.Length ~.,
      data = iris_df
    )
  )

  expect_error(
    fit_xy(
      object = iris_neuralnet,
      x = iris_df[, 2:5],
      y = iris_df$Sepal.Length
    )
  )

})

# classification using neuralnet -----------------------------------------------
test_that('neuralnet classification', {

  skip_if_not_installed("neuralnet")

  # classification using neural net
  set.seed(1234)
  nn_model <-
    neuralnet::neuralnet(
      formula = Species ~ .,
      data = iris_df,
      hidden = 5,
      linear.output = FALSE
    )

  nn_probs <- predict(nn_model, iris_df)
  colnames(nn_probs) <- paste0(".pred_", levels(iris_df$Species))
  nn_probs <- tibble::as_tibble(nn_probs)

  nn_preds <-
    tibble(.pred_class = levels(iris_df$Species)[apply(nn_probs, 1, which.max)])
  nn_preds$.pred_class <-
    factor(nn_preds$.pred_class, levels = levels(iris_df$Species))

  # classification using parsnip formula
  clf <- mlp(mode = "classification", hidden_units = 5) %>%
    set_engine("neuralnet")

  set.seed(1234)
  clf_fit_form <- fit(clf, Species ~., iris_df)
  parsnip_probs_form <- predict(clf_fit_form, iris_df, type = "prob")
  parsnip_preds_form <- predict(clf_fit_form, iris_df)

  set.seed(1234)
  clf_fit_xy <- fit_xy(clf, x = iris_df[, 1:4], y = iris_df$Species)
  parsnip_probs_xy <- predict(clf_fit_xy, iris_df[, 1:4], type = "prob")
  parsnip_preds_xy <- predict(clf_fit_xy, iris_df[, 1:4])

  # test that predictions are equal
  testthat::expect_equal(nn_probs, parsnip_probs_form)
  testthat::expect_equal(parsnip_probs_form, parsnip_probs_xy)

  testthat::expect_equal(nn_preds, parsnip_preds_form)
  testthat::expect_equal(parsnip_preds_form, parsnip_preds_xy)
})

# neuralnet regression ---------------------------------------------------------
test_that('neuralnet regression', {

  skip_if_not_installed("neuralnet")

  # classification using neuralnet
  set.seed(1234)
  nn_model <- neuralnet::neuralnet(
    formula = Petal.Length ~ .,
    data = iris_df[, 1:4],
    hidden = 5,
    linear.output = TRUE
  )

  nn_preds <- predict(nn_model, iris_df[, 1:4])
  colnames(nn_preds) <- ".pred"
  nn_preds <- as_tibble(nn_preds)

  # classification using parsnip
  regr <- mlp(mode = "regression", hidden_units = 5, epochs = 5) %>%
    set_engine("neuralnet")

  set.seed(1234)
  regr_fit_form <- fit(regr, Petal.Length ~., iris_df[, 1:4])
  parsnip_preds_form <- predict(regr_fit_form, iris_df[, 1:4])

  set.seed(1234)
  regr_fit_xy <- fit_xy(regr, x = iris_df[, c(1, 2, 4)], y = iris_df$Petal.Length)
  parsnip_preds_xy <- predict(regr_fit_xy, iris_df[, 1:4])

  # test that predictions are equal
  testthat::expect_equal(nn_preds, parsnip_preds_form)
  testthat::expect_equal(parsnip_preds_form, parsnip_preds_xy)
})
