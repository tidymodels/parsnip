library(testthat)
library(parsnip)
library(rlang)

# ------------------------------------------------------------------------------

context("simple neural networks")
source("helpers.R")

# ------------------------------------------------------------------------------


test_that('primary arguments', {
  hidden_units <- mlp(mode = "regression", hidden_units = 4)
  hidden_units_nnet <- translate(hidden_units %>% set_engine("nnet"))
  hidden_units_keras <- translate(hidden_units %>% set_engine("keras"))
  expect_equal(hidden_units_nnet$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 size = new_empty_quosure(4),
                 trace = FALSE,
                 linout = TRUE
               )
  )
  expect_equal(hidden_units_keras$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 hidden_units = new_empty_quosure(4)
               )
  )

  no_hidden_units <- mlp(mode = "regression")
  no_hidden_units_nnet <- translate(no_hidden_units %>% set_engine("nnet"))
  expect_equal(no_hidden_units_nnet$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 size = 5,
                 trace = FALSE,
                 linout = TRUE
               )
  )
  expect_equal(hidden_units_keras$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 hidden_units = new_empty_quosure(4)
               )
  )

  all_args <-
    mlp(
      mode = "classification",
      epochs = 2, hidden_units = 4, penalty = 0.0001,
      dropout = 0, activation = "softmax"
    )
  all_args_nnet <- translate(all_args %>% set_engine("nnet"))
  all_args_keras <- translate(all_args %>% set_engine("keras"))
  expect_equal(all_args_nnet$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 size = new_empty_quosure(4),
                 decay = new_empty_quosure(1e-04),
                 maxit = new_empty_quosure(2),
                 trace = FALSE,
                 linout = FALSE
               )
  )
  expect_equal(all_args_keras$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 hidden_units = new_empty_quosure(4),
                 penalty = new_empty_quosure(1e-04),
                 dropout = new_empty_quosure(0),
                 epochs = new_empty_quosure(2),
                 activation = new_empty_quosure("softmax")
               )
  )

})

test_that('engine arguments', {
  nnet_hess <- mlp(mode = "classification") %>% set_engine("nnet", Hess = TRUE)
  expect_equal(translate(nnet_hess)$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 size = 5,
                 Hess = new_empty_quosure(TRUE),
                 trace = FALSE,
                 linout = FALSE
               )
  )

  keras_val <- mlp(mode = "regression") %>% set_engine("keras", validation_split = 0.2)
  expect_equal(translate(keras_val)$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 validation_split = new_empty_quosure(0.2)
               )
  )


  nnet_tol <- mlp(mode = "regression") %>% set_engine("nnet", abstol = varying())
  expect_equal(translate(nnet_tol)$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 size = 5,
                 abstol = new_empty_quosure(varying()),
                 trace = FALSE,
                 linout = TRUE
               )
  )
})


test_that('updating', {
  expr1     <- mlp(mode = "regression") %>%
    set_engine("nnet", Hess = FALSE, abstol = varying())
  expr1_exp <- mlp(mode = "regression", hidden_units = 2) %>%
    set_engine("nnet", Hess = FALSE, abstol = varying())

  expr2     <- mlp(mode = "regression", hidden_units = 7) %>% set_engine("nnet")
  expr2_exp <- mlp(mode = "regression", hidden_units = 7) %>% set_engine("nnet", Hess = FALSE)

  expr3     <- mlp(mode = "regression", hidden_units = 7, epochs = varying()) %>% set_engine("keras")

  expr3_exp <- mlp(mode = "regression", hidden_units = 2) %>% set_engine("keras")

  expr4     <- mlp(mode = "classification", hidden_units = 2) %>% set_engine("nnet", Hess = FALSE, abstol = varying())
  expr4_exp <- mlp(mode = "classification", hidden_units = 2) %>% set_engine("nnet", Hess = FALSE, abstol = varying())

  expr5     <- mlp(mode = "classification", hidden_units = 2) %>% set_engine("nnet", Hess = FALSE)
  expr5_exp <- mlp(mode = "classification", hidden_units = 2) %>% set_engine("nnet", Hess = FALSE, abstol = varying())

  expect_equal(update(expr1, hidden_units = 2), expr1_exp)
  expect_equal(update(expr3, hidden_units = 2, fresh = TRUE), expr3_exp)

})

test_that('bad input', {
  expect_error(mlp(mode = "time series"))
  expect_error(translate(mlp(mode = "classification") %>% set_engine("wat?")))
  expect_warning(translate(mlp(mode = "regression") %>% set_engine("nnet", formula = y ~ x)))
  expect_error(translate(mlp(mode = "classification", x = x, y = y) %>% set_engine("keras")))
  expect_error(translate(mlp(mode = "regression", formula = y ~ x) %>% set_engine()))
})

