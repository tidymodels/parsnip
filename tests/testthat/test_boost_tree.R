library(testthat)
library(parsnip)
library(rlang)

# ------------------------------------------------------------------------------

context("boosted trees")
source("helpers.R")
source(test_path("helper-objects.R"))
hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

test_that('updating', {
  expr1     <- boost_tree() %>% set_engine("xgboost", verbose = 0)
  expr1_exp <- boost_tree(trees = 10) %>% set_engine("xgboost", verbose = 0)

  expr2     <- boost_tree(trees = tune()) %>% set_engine("C5.0", bands = tune())
  expr2_exp <- boost_tree(trees = tune())  %>% set_engine("C5.0", bands = 10)

  expr3     <- boost_tree(trees = 1, sample_size = tune())
  expr3_exp <- boost_tree(trees = 1)

  expr4     <- boost_tree() %>% set_engine("C5.0", noGlobalPruning = tune())
  expr4_exp <- boost_tree() %>% set_engine("C5.0", noGlobalPruning = TRUE)

  expect_equal(update(expr1, trees = 10), expr1_exp)
  expect_equal(update(expr2, bands = 10), expr2_exp)
  expect_equal(update(expr3, trees = 1, fresh = TRUE), expr3_exp)
  expect_equal(update(expr4, noGlobalPruning = TRUE), expr4_exp)

  param_tibb <- tibble::tibble(trees = 7, mtry = 1)
  param_list <- as.list(param_tibb)

  expr1_updated <- update(expr1, param_tibb)
  expect_equal(expr1_updated$args$trees, 7)
  expect_equal(expr1_updated$args$mtry, 1)
  expect_equal(expr1_updated$eng_args$verbose, rlang::quo(0))

  expr1_updated_lst <- update(expr1, param_list)
  expect_equal(expr1_updated_lst$args$trees, 7)
  expect_equal(expr1_updated_lst$args$mtry, 1)
  expect_equal(expr1_updated_lst$eng_args$verbose, rlang::quo(0))
})

test_that('bad input', {
  expect_error(boost_tree(mode = "bogus"))
  expect_error({
    bt <- boost_tree(trees = -1) %>% set_engine("xgboost")
    fit(bt, class ~ ., hpc)
  })
  expect_error({
    bt <- boost_tree(min_n = -10)  %>% set_engine("xgboost")
    fit(bt, class ~ ., hpc)
  })
  expect_message(translate(boost_tree(mode = "classification"), engine = NULL))
  expect_error(translate(boost_tree(formula = y ~ x)))
})


## -----------------------------------------------------------------------------

test_that('argument checks for data dimensions', {

  spec <-
    boost_tree(mtry = 1000, min_n = 1000, trees = 5) %>%
    set_engine("spark") %>%
    set_mode("classification")

  args <- translate(spec)$method$fit$args
  expect_equal(args$min_instances_per_node, expr(min_rows(1000, x)))
})

