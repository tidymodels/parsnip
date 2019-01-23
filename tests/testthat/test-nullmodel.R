library(testthat)
library(parsnip)
library(rlang)
library(tibble)

context("test-nullmodel")
source("helpers.R")


test_that('primary arguments', {
  basic <- nullmodel(mode = "regression")
  basic_nullmodel <- translate(basic %>% set_engine("parsnip"))
  expect_equal(basic_nullmodel$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg())
               )
  )
})


test_that('engine arguments', {
  nullmodel_keep <- nullmodel(mode = "regression")
  expect_equal(translate(nullmodel_keep %>% set_engine("parsnip", keepxy = FALSE))$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 keepxy = new_empty_quosure(FALSE)
               )
  )
})

test_that('bad input', {
  expect_error(translate(nullmodel() %>% set_engine("wat?")))
  expect_error(translate(nullmodel(mode = "regression") %>% set_engine()))
  expect_error(translate(nullmodel(formula = y ~ x)))
  expect_warning(
    translate(
      nullmodel(mode = "regression") %>% set_engine("parsnip", x = iris[,1:3], y = iris$Species)
    )
  )
})

# ------------------------------------------------------------------------------

num_pred <- c("Sepal.Width", "Petal.Width", "Petal.Length")
iris_bad_form <- as.formula(Species ~ term)
iris_basic <- nullmodel(mode = "regression") %>% set_engine("parsnip")

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
      y = iris$Sepal.Length
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

})

test_that('nullmodel prediction', {

  uni_pred <- tibble(.pred = rep(5.843333, 5))
  inl_pred <- rep(5.843333, 5)

  mv_pred <-
    structure(
      list(Sepal.Width = seq_len(10),
           Petal.Width = seq_len(10)),
      class = "data.frame",
      row.names = c(NA, -10L))

  res_xy <- fit_xy(
    iris_basic,
    x = iris[, num_pred],
    y = iris$Sepal.Length
  )

  expect_equal(uni_pred, predict(res_xy, new_data = iris[1, num_pred]))

  res_form <- fit(
    iris_basic,
    Sepal.Length ~ log(Sepal.Width) + Species,
    data = iris
  )
  expect_equal(inl_pred, predict_numeric(res_form, iris[1:5, ]))
})

# ------------------------------------------------------------------------------

test_that('classification', {

  expect_error(
    nullmodel <- nullmodel(mode = "classification") %>%
      set_engine("parsnip") %>%
      fit(Species ~ ., data = iris),
    regexp = NA
  )
  expect_true(!is.null(nullmodel$fit))
})

