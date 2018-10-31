library(testthat)
library(parsnip)
library(rlang)

# ------------------------------------------------------------------------------

context("decision trees")
source("helpers.R")

# ------------------------------------------------------------------------------

test_that('primary arguments', {
  basic <- decision_tree(mode = "classification")
  basic_rpart <- translate(basic %>% set_engine("rpart"))
  basic_C5.0 <- translate(basic %>% set_engine("C5.0"))
  expect_equal(basic_rpart$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg())
               )
  )
  expect_equal(basic_C5.0$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 trials = 1
               )
  )

  cost_complexity <- decision_tree(cost_complexity = 15, mode = "classification")
  cost_complexity_rpart <- translate(cost_complexity %>% set_engine("rpart"))
  expect_equal(cost_complexity_rpart$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 cp = new_empty_quosure(15)
               )
  )

  split_num <- decision_tree(min_n = 15, mode = "classification")
  split_num_C5.0 <- translate(split_num %>% set_engine("C5.0"))
  split_num_rpart <- translate(split_num %>% set_engine("rpart"))
  expect_equal(split_num_C5.0$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 minCases = new_empty_quosure(15),
                 trials = 1
               )
  )
  expect_equal(split_num_rpart$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 minsplit = new_empty_quosure(15)
               )
  )

})

test_that('engine arguments', {
  rpart_print <- decision_tree(mode = "regression")
  expect_equal(
    translate(
      rpart_print %>%
        set_engine("rpart", model = TRUE))$method$fit$args,
    list(
      formula = expr(missing_arg()),
      data = expr(missing_arg()),
      weights = expr(missing_arg()),
      model = new_empty_quosure(TRUE)
    )
  )

  C5.0_rules <- decision_tree(mode = "classification")
  expect_equal(
    translate(
      C5.0_rules %>% set_engine("C5.0", rules = TRUE))$method$fit$args,
    list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      weights = expr(missing_arg()),
      rules = new_empty_quosure(TRUE),
      trials = 1
    )
  )

})


test_that('updating', {
  expr1     <- decision_tree() %>% set_engine("rpart", model = FALSE)
  expr1_exp <- decision_tree(cost_complexity = .1) %>% set_engine("rpart", model = FALSE)

  expr2     <- decision_tree(cost_complexity = varying()) %>% set_engine("rpart")
  expr2_exp <- decision_tree(cost_complexity = varying())  %>% set_engine("rpart", model = FALSE)

  expr3     <- decision_tree(cost_complexity = 1, min_n = varying())
  expr3_exp <- decision_tree(cost_complexity = 1)

  expect_equal(update(expr1, cost_complexity = .1), expr1_exp)
  expect_equal(update(expr3, cost_complexity = 1, fresh = TRUE), expr3_exp)
})

test_that('bad input', {
  expect_error(decision_tree(mode = "bogus"))
  expect_error({
    bt <- decision_tree(cost_complexity = -1)
    fit(bt, Species ~ ., iris) %>% set_engine("rpart")
  })
  expect_error({
    bt <- decision_tree(min_n = -10)
    fit(bt, Species ~ ., iris)  %>% set_engine("rpart")
  })
  expect_error(translate(decision_tree(), engine = NULL))
  expect_error(translate(decision_tree(formula = y ~ x)))
})

# ------------------------------------------------------------------------------
