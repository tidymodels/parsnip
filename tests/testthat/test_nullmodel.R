library(testthat)
library(parsnip)
library(rlang)
library(tibble)

context("test-nullmodel")
source("helpers.R")


test_that('primary arguments', {
  basic <- null_model(mode = "regression")
  basic_nullmodel <- translate(basic %>% set_engine("parsnip"))
  expect_equal(basic_nullmodel$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg())
               )
  )
})


test_that('engine arguments', {
  nullmodel_keep <- null_model(mode = "regression")
  expect_equal(translate(nullmodel_keep %>% set_engine("parsnip", keepxy = FALSE))$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 keepxy = new_empty_quosure(FALSE)
               )
  )
})

test_that('bad input', {
  expect_error(translate(null_model() %>% set_engine("wat?")))
  expect_error(translate(null_model(mode = "regression") %>% set_engine()))
  expect_error(translate(null_model(formula = y ~ x)))
  expect_warning(
    translate(
      null_model(mode = "regression") %>% set_engine("parsnip", x = iris[,1:3], y = iris$Species)
    )
  )
})

# ------------------------------------------------------------------------------

num_pred <-c("Sepal.Length", "Sepal.Width", "Petal.Width")
iris_bad_form <- as.formula(Species ~ term)
iris_basic <- null_model(mode = "regression") %>% set_engine("parsnip")

# ------------------------------------------------------------------------------

test_that('nullmodel execution', {

  expect_error(
    res <- fit(
      iris_basic,
      Sepal.Length ~ log(Sepal.Width) + Species,
      data = iris
    ),
    regexp = NA
  )
  expect_error(
    res <- fit_xy(
      iris_basic,
      x = iris[, num_pred],
      y = iris$Petal.Length
    ),
    regexp = NA
  )

  expect_error(
    res <- fit(
      iris_basic,
      iris_bad_form,
      data = iris
    )
  )

  ## multivariate y

  expect_error(
    res <- fit(
      iris_basic,
      cbind(Sepal.Width, Petal.Width) ~ .,
      data = iris
    ),
    regexp = NA
  )

})

test_that('nullmodel prediction', {

  uni_pred <- tibble(.pred = rep(3.758, 5))
  inl_pred <- rep(3.758, 5)
  mw_pred <- tibble(gear = rep(3.6875, 5),
                    carb = rep(2.8125, 5))

  res_xy <- fit_xy(
    iris_basic,
    x = iris[, num_pred],
    y = iris$Petal.Length
  )

  expect_equal(uni_pred, predict(res_xy, new_data = iris[1:5, num_pred]))

  res_form <- fit(
    iris_basic,
    Petal.Length ~ log(Sepal.Width) + Species,
    data = iris
  )
  expect_equal(inl_pred, predict(res_form, iris[1:5, ])$.pred)

  # Multivariate y
  res <- fit(
    iris_basic,
    cbind(gear, carb) ~ .,
    data = mtcars
  )

  expect_equal(
    setNames(mw_pred, paste0(".pred_", names(mw_pred))),
    predict(res, mtcars[1:5, ])
  )
})

# ------------------------------------------------------------------------------

test_that('classification', {

  expect_error(
    null_model <- null_model(mode = "classification") %>%
      set_engine("parsnip") %>%
      fit(Species ~ ., data = iris),
    regexp = NA
  )
  expect_true(!is.null(null_model$fit))
})


