library(testthat)
library(parsnip)
library(rlang)

# ------------------------------------------------------------------------------

context("boosted trees")
source("helpers.R")

# ------------------------------------------------------------------------------

test_that('primary arguments', {
  basic <- boost_tree(mode = "classification")
  basic_xgboost <- translate(basic %>% set_engine("xgboost"))
  basic_C5.0 <- translate(basic %>% set_engine("C5.0"))
  expect_equal(basic_xgboost$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 nthread = 1,
                 verbose = 0
               )
  )
  expect_equal(basic_C5.0$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg())
               )
  )

  trees <- boost_tree(trees = 15, mode = "classification")
  trees_C5.0 <- translate(trees %>% set_engine("C5.0"))
  trees_xgboost <- translate(trees %>% set_engine("xgboost"))
  expect_equal(trees_C5.0$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 trials = new_empty_quosure(15)
               )
  )
  expect_equal(trees_xgboost$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 nrounds = new_empty_quosure(15),
                 nthread = 1,
                 verbose = 0
               )
  )

  split_num <- boost_tree(min_n = 15, mode = "classification")
  split_num_C5.0 <- translate(split_num %>% set_engine("C5.0"))
  split_num_xgboost <- translate(split_num %>% set_engine("xgboost"))
  expect_equal(split_num_C5.0$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 minCases = new_empty_quosure(15)
               )
  )
  expect_equal(split_num_xgboost$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 min_child_weight = new_empty_quosure(15),
                 nthread = 1,
                 verbose = 0
               )
  )

})

test_that('engine arguments', {
  xgboost_print <- boost_tree(mode = "regression")
  expect_equal(
    translate(
      xgboost_print %>%
        set_engine("xgboost", print_every_n = 10L))$method$fit$args,
    list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      print_every_n = new_empty_quosure(10L),
      nthread = 1,
      verbose = 0
    )
  )

  C5.0_rules <- boost_tree(mode = "classification")
  expect_equal(
    translate(
      C5.0_rules %>% set_engine("C5.0", rules = TRUE))$method$fit$args,
    list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      weights = expr(missing_arg()),
      rules = new_empty_quosure(TRUE)
    )
  )

})


test_that('updating', {
  expr1     <- boost_tree() %>% set_engine("xgboost", verbose = 0)
  expr1_exp <- boost_tree(trees = 10) %>% set_engine("xgboost", verbose = 0)

  expr2     <- boost_tree(trees = varying()) %>% set_engine("xgboost")
  expr2_exp <- boost_tree(trees = varying())  %>% set_engine("xgboost", verbose = 0)

  expr3     <- boost_tree(trees = 1, sample_size = varying())
  expr3_exp <- boost_tree(trees = 1)

  expect_equal(update(expr1, trees = 10), expr1_exp)
  expect_equal(update(expr3, trees = 1, fresh = TRUE), expr3_exp)
})

test_that('bad input', {
  expect_error(boost_tree(mode = "bogus"))
  expect_error({
    bt <- boost_tree(trees = -1)
    fit(bt, Species ~ ., iris) %>% set_engine("xgboost")
  })
  expect_error({
    bt <- boost_tree(min_n = -10)
    fit(bt, Species ~ ., iris)  %>% set_engine("xgboost")
  })
  expect_message(translate(boost_tree(), engine = NULL))
  expect_error(translate(boost_tree(formula = y ~ x)))
})

# ------------------------------------------------------------------------------
