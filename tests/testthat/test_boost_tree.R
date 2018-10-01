library(testthat)
context("boosted trees")
library(parsnip)
library(rlang)

test_that('primary arguments', {
  basic <- boost_tree(mode = "classification")
  basic_xgboost <- translate(basic, engine = "xgboost")
  basic_C5.0 <- translate(basic, engine = "C5.0")
  expect_equal(basic_xgboost$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 nthread = 1,
                 verbose = 0
               )
  )
  expect_equal(basic_C5.0$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg())
               )
  )

  trees <- boost_tree(trees = 15, mode = "classification")
  trees_C5.0 <- translate(trees, engine = "C5.0")
  trees_xgboost <- translate(trees, engine = "xgboost")
  expect_equal(trees_C5.0$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 trials = 15
               )
  )
  expect_equal(trees_xgboost$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 nrounds = 15,
                 nthread = 1,
                 verbose = 0
               )
  )

  split_num <- boost_tree(min_n = 15, mode = "classification")
  split_num_C5.0 <- translate(split_num, engine = "C5.0")
  split_num_xgboost <- translate(split_num, engine = "xgboost")
  expect_equal(split_num_C5.0$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 minCases = 15
               )
  )
  expect_equal(split_num_xgboost$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 min_child_weight = 15,
                 nthread = 1,
                 verbose = 0
               )
  )

})

test_that('engine arguments', {
  xgboost_print <- boost_tree(mode = "regression", others = list(print_every_n = 10L))
  expect_equal(translate(xgboost_print, engine = "xgboost")$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 print_every_n = 10L,
                 nthread = 1,
                 verbose = 0
               )
  )

  C5.0_rules <- boost_tree(mode = "classification", others = list(rules = TRUE))
  expect_equal(translate(C5.0_rules, engine = "C5.0")$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 weights = quote(missing_arg()),
                 rules = TRUE
               )
  )

})


test_that('updating', {
  expr1     <- boost_tree(             others = list(verbose = 0))
  expr1_exp <- boost_tree(trees = 10,  others = list(verbose = 0))

  expr2     <- boost_tree(trees = varying())
  expr2_exp <- boost_tree(trees = varying(), others = list(verbose = 0))

  expr3     <- boost_tree(trees = 1, sample_size = varying())
  expr3_exp <- boost_tree(trees = 1)

  expr4     <- boost_tree(trees = 10, others = list(rules = TRUE))
  expr4_exp <- boost_tree(trees = 10, others = list(rules = TRUE, earlyStopping = TRUE))

  expr5     <- boost_tree(trees = 1, others = list(rules = TRUE, earlyStopping = TRUE))

  expect_equal(update(expr1, trees = 10), expr1_exp)
  expect_equal(update(expr2, others = list(verbose = 0)), expr2_exp)
  expect_equal(update(expr3, trees = 1, fresh = TRUE), expr3_exp)
  expect_equal(update(expr4, others = list(rules = TRUE, earlyStopping = TRUE)), expr4_exp)
  expect_equal(update(expr5, others = list(rules = TRUE)), expr5)

})

test_that('bad input', {
  expect_error(boost_tree(ase.weights = var))
  expect_error(boost_tree(mode = "bogus"))
  expect_error(boost_tree(trees = -1))
  expect_error(boost_tree(min_n = -10))
  expect_error(translate(boost_tree(), engine = "wat?"))
  expect_warning(translate(boost_tree(), engine = NULL))
  expect_error(translate(boost_tree(formula = y ~ x)))
})

###################################################################
