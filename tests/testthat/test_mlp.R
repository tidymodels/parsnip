library(testthat)
context("simple neural networks")
library(parsnip)

test_that('primary arguments', {
  hidden_units <- mlp(mode = "regression", hidden_units = 4)
  hidden_units_nnet <- translate(hidden_units, engine = "nnet")
  hidden_units_keras <- translate(hidden_units, engine = "keras")
  expect_equal(hidden_units_nnet$method$fit$args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 size = 4,
                 trace = FALSE,
                 linout = TRUE
               )
  )
  expect_equal(hidden_units_keras$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 hidden_units = 4
               )
  )

  no_hidden_units <- mlp(mode = "regression")
  no_hidden_units_nnet <- translate(no_hidden_units, engine = "nnet")
  expect_equal(no_hidden_units_nnet$method$fit$args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 size = 5,
                 trace = FALSE,
                 linout = TRUE
               )
  )
  expect_equal(hidden_units_keras$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 hidden_units = 4
               )
  )

  all_args <-
    mlp(
      mode = "classification",
      epochs = 2, hidden_units = 4, penalty = 0.0001,
      dropout = 0, activation = "softmax"
    )
  all_args_nnet <- translate(all_args, engine = "nnet")
  all_args_keras <- translate(all_args, engine = "keras")
  expect_equal(all_args_nnet$method$fit$args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 size = 4,
                 decay = 1e-04,
                 maxit = 2,
                 trace = FALSE,
                 linout = FALSE
               )
  )
  expect_equal(all_args_keras$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 hidden_units = 4,
                 penalty = 1e-04,
                 dropout = 0,
                 epochs = 2,
                 activation = "softmax"
               )
  )

})

test_that('engine arguments', {
  nnet_hess <- mlp(mode = "classification", others = list(Hess = TRUE))
  expect_equal(translate(nnet_hess, engine = "nnet")$method$fit$args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 size = 5,
                 Hess = TRUE,
                 trace = FALSE,
                 linout = FALSE
               )
  )

  keras_val <- mlp(mode = "regression", others = list(validation_split = 0.2))
  expect_equal(translate(keras_val, engine = "keras")$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 validation_split = 0.2
               )
  )


  nnet_tol <- mlp(mode = "regression", others = list(abstol = varying()))
  expect_equal(translate(nnet_tol, engine = "nnet")$method$fit$args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 size = 5,
                 abstol = varying(),
                 trace = FALSE,
                 linout = TRUE
               )
  )
})


test_that('updating', {
  expr1     <- mlp(mode = "regression",            others = list(Hess = FALSE, abstol = varying()))
  expr1_exp <- mlp(mode = "regression", hidden_units = 2, others = list(Hess = FALSE, abstol = varying()))

  expr2     <- mlp(mode = "regression", hidden_units = 7)
  expr2_exp <- mlp(mode = "regression", hidden_units = 7, others = list(Hess = FALSE))

  expr3     <- mlp(mode = "regression", hidden_units = 7, epochs = varying())

  expr3_exp <- mlp(mode = "regression", hidden_units = 2)

  expr4     <- mlp(mode = "classification", hidden_units = 2, others = list(Hess = TRUE, abstol = varying()))
  expr4_exp <- mlp(mode = "classification", hidden_units = 2, others = list(Hess = FALSE, abstol = varying()))

  expr5     <- mlp(mode = "classification", hidden_units = 2, others = list(Hess = FALSE))
  expr5_exp <- mlp(mode = "classification", hidden_units = 2, others = list(Hess = FALSE, abstol = varying()))

  expect_equal(update(expr1, hidden_units = 2), expr1_exp)
  expect_equal(update(expr2, others = list(Hess = FALSE)), expr2_exp)
  expect_equal(update(expr3, hidden_units = 2, fresh = TRUE), expr3_exp)
  expect_equal(update(expr4, others = list(Hess = FALSE)), expr4_exp)
  expect_equal(update(expr5, others = list(Hess = FALSE, abstol = varying())), expr5_exp)

})

test_that('bad input', {
  expect_error(mlp(mode = "classification", weights = var))
  expect_error(mlp(mode = "time series"))
  expect_error(translate(mlp(mode = "classification"), engine = "wat?"))
  expect_error(translate(mlp(mode = "classification", others = list(ytest = 2))))
  expect_error(translate(mlp(mode = "regression", formula = y ~ x)))
  expect_error(translate(mlp(mode = "classification", others = list(x = x, y = y)), engine = "keras"))
  expect_error(translate(mlp(mode = "regression", others = list(formula = y ~ x)), engine = ""))
})

