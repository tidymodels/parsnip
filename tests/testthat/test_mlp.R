library(testthat)
library(parsnip)

test_that('primary arguments', {
  units <- mlp(mode = "regression", units = 4)
  units_nnet <- translate(units, engine = "nnet")
  units_keras <- translate(units, engine = "keras")
  expect_equal(units_nnet$method$fit$args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 size = 4,
                 trace = FALSE
               )
  )
  expect_equal(units_keras$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 units = 4
               )
  )

  no_units <- mlp(mode = "regression")
  no_units_nnet <- translate(no_units, engine = "nnet")
  expect_equal(no_units_nnet$method$fit$args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 size = 5,
                 trace = FALSE
               )
  )
  expect_equal(units_keras$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 units = 4
               )
  )

  all_args <-
    mlp(
      mode = "classification",
      epochs = 2, units = 4, weight_decay = 0.0001,
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
                 units = 4,
                 weight_decay = 1e-04,
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
                 trace = FALSE
               )
  )
})


test_that('updating', {
  expr1     <- mlp(mode = "regression",            others = list(Hess = FALSE, abstol = varying()))
  expr1_exp <- mlp(mode = "regression", units = 2, others = list(Hess = FALSE, abstol = varying()))

  expr2     <- mlp(mode = "regression", units = 7)
  expr2_exp <- mlp(mode = "regression", units = 7, others = list(Hess = FALSE))

  expr3     <- mlp(mode = "regression", units = 7, epochs = varying())
  expr3_exp <- mlp(mode = "regression", units = 2)

  expr4     <- mlp(mode = "classification", units = 2, others = list(Hess = TRUE, abstol = varying()))
  expr4_exp <- mlp(mode = "classification", units = 2, others = list(Hess = FALSE, abstol = varying()))

  expr5     <- mlp(mode = "classification", units = 2, others = list(Hess = FALSE))
  expr5_exp <- mlp(mode = "classification", units = 2, others = list(Hess = FALSE, abstol = varying()))

  expect_equal(update(expr1, units = 2), expr1_exp)
  expect_equal(update(expr2, others = list(Hess = FALSE)), expr2_exp)
  expect_equal(update(expr3, units = 2, fresh = TRUE), expr3_exp)
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


###################################################################

iris_form <- as.formula(Species ~ Sepal.Width + Sepal.Length)
num_pred <- names(iris)[1:4]
iris_bad_form <- as.formula(Species ~ novar)

iris_nnet <- mlp(mode = "classification", units = 2)

ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)


test_that('nnet execution, classification', {
  skip_on_cran()
  skip_if_not_installed("nnet")

  # passes interactively but not on R CMD check
  expect_error(
    res <- parsnip::fit(
      iris_nnet,
      iris_form,
      data = iris,
      engine = "nnet",
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- parsnip::fit(
      iris_nnet,
      x = iris[, num_pred],
      y = iris$Species,
      engine = "nnet",
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    res <- parsnip::fit(
      iris_nnet,
      iris_bad_form,
      data = iris,
      engine = "nnet",
      control = ctrl
    )
  )
})

test_that('keras execution, classification', {
  skip_on_cran()
  skip_if_not_installed("keras")

  # passes interactively but not on R CMD check
  expect_error(
    parsnip::fit(
      iris_nnet,
      iris_form,
      data = iris,
      engine = "keras",
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    parsnip::fit(
      iris_nnet,
      engine = "keras",
      control = ctrl,
      x = iris[, num_pred],
      y = iris$Species
    ),
    regexp = NA
  )

  expect_error(
    parsnip::fit(
      iris_nnet,
      iris_bad_form,
      data = iris,
      engine = "keras",
      control = ctrl
    )
  )
})

###################################################################

car_form <- as.formula(mpg ~ .)
num_pred <- names(mtcars)[3:6]

car_basic <- mlp(mode = "regression")

bad_nnet_reg <- mlp(mode = "regression",
                    others = list(min.node.size = -10))
bad_rf_reg <- mlp(mode = "regression",
                  others = list(sampsize = -10))

ctrl <- list(verbosity = 1, catch = FALSE)
caught_ctrl <- list(verbosity = 1, catch = TRUE)
quiet_ctrl <- list(verbosity = 0, catch = TRUE)

test_that('nnet execution, regression', {
  skip_on_cran()
  skip_if_not_installed("nnet")

  expect_error(
    res <- parsnip::fit(
      car_basic,
      car_form,
      data = mtcars,
      engine = "nnet",
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    res <- parsnip::fit(
      car_basic,
      x = mtcars[, num_pred],
      y = mtcars$mpg,
      engine = "nnet",
      control = ctrl
    ),
    regexp = NA
  )
})

test_that('keras execution', {
  skip_on_cran()
  skip_if_not_installed("keras")

  # passes interactively but not on R CMD check
  expect_error(
    parsnip::fit(
      car_basic,
      car_form,
      data = mtcars,
      engine = "keras",
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    parsnip::fit(
      car_basic,
      x = mtcars[, num_pred],
      y = mtcars$mpg,
      engine = "keras",
      control = ctrl
    ),
    regexp = NA
  )
})

