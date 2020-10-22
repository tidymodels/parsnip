library(testthat)
library(parsnip)
library(rlang)

# ------------------------------------------------------------------------------

context("random forest models")
source("helpers.R")

# ------------------------------------------------------------------------------

test_that('primary arguments', {
  mtry <- rand_forest(mode = "regression", mtry = 4)
  mtry_ranger <- translate(mtry %>% set_engine("ranger"))
  mtry_randomForest <- translate(mtry %>% set_engine("randomForest"))
  mtry_spark <- translate(mtry %>% set_engine("spark"))
  expect_equal(mtry_ranger$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 case.weights = expr(missing_arg()),
                 mtry = expr(min_cols(~4, x)),
                 num.threads = 1,
                 verbose = FALSE,
                 seed = expr(sample.int(10^5, 1))
               )
  )
  expect_equal(mtry_randomForest$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 mtry = expr(min_cols(~4, x))
               )
  )
  expect_equal(mtry_spark$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 formula = expr(missing_arg()),
                 type = "regression",
                 feature_subset_strategy = "4",
                 seed = expr(sample.int(10^5, 1))
               )
  )
  trees <- rand_forest(mode = "classification", trees = 1000)
  trees_ranger <- translate(trees %>% set_engine("ranger"))
  trees_randomForest <- translate(trees %>% set_engine("randomForest"))
  trees_spark <- translate(trees %>% set_engine("spark"))
  expect_equal(trees_ranger$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 case.weights = expr(missing_arg()),
                 num.trees = new_empty_quosure(1000),
                 num.threads = 1,
                 verbose = FALSE,
                 seed = expr(sample.int(10^5, 1)),
                 probability = TRUE
               )
  )
  expect_equal(trees_randomForest$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 ntree = new_empty_quosure(1000)
               )
  )
  expect_equal(trees_spark$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 formula = expr(missing_arg()),
                 type = "classification",
                 num_trees = new_empty_quosure(1000),
                 seed = expr(sample.int(10^5, 1))
               )
  )

  min_n <- rand_forest(mode = "regression", min_n = 5)
  min_n_ranger <- translate(min_n %>% set_engine("ranger"))
  min_n_randomForest <- translate(min_n %>% set_engine("randomForest"))
  min_n_spark <- translate(min_n %>% set_engine("spark"))
  expect_equal(min_n_ranger$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 case.weights = expr(missing_arg()),
                 min.node.size = expr(min_rows(~5, x)),
                 num.threads = 1,
                 verbose = FALSE,
                 seed = expr(sample.int(10^5, 1))
               )
  )
  expect_equal(min_n_randomForest$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 nodesize = expr(min_rows(~5, x))
               )
  )
  expect_equal(min_n_spark$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 formula = expr(missing_arg()),
                 type = "regression",
                 min_instances_per_node = expr(min_rows(~5, x)),
                 seed = expr(sample.int(10^5, 1))
               )
  )
})

test_that('engine arguments', {
  ranger_imp <- rand_forest(mode = "classification")
  expect_equal(translate(ranger_imp %>% set_engine("ranger", importance = "impurity"))$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 case.weights = expr(missing_arg()),
                 importance = new_empty_quosure("impurity"),
                 num.threads = 1,
                 verbose = FALSE,
                 seed = expr(sample.int(10^5, 1)),
                 probability = TRUE
               )
  )

  randomForest_votes <- rand_forest(mode = "regression")
  expect_equal(translate(randomForest_votes %>% set_engine("randomForest", norm.votes = FALSE))$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 norm.votes = new_empty_quosure(FALSE)
               )
  )

  spark_gain <- rand_forest(mode = "regression")
  expect_equal(translate(spark_gain %>% set_engine("spark", min_info_gain = 2))$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 formula = expr(missing_arg()),
                 type = "regression",
                 min_info_gain = new_empty_quosure(2),
                 seed = expr(sample.int(10^5, 1))
               )
  )

})


test_that('updating', {
  expr1     <- rand_forest(mode = "regression") %>%
    set_engine("randomForest",  norm.votes = FALSE, sampsize = varying())
  expr1_exp <- rand_forest(mode = "regression", mtry = 2) %>%
    set_engine("randomForest", norm.votes = FALSE, sampsize = varying())

  expr2     <- rand_forest(mode = "regression", mtry = 7, min_n = varying()) %>%
    set_engine("randomForest")
  expr2_exp <- rand_forest(mode = "regression", mtry = 7, min_n = varying() %>%
                             set_engine("randomForest", norm.votes = FALSE))

  expr3     <- rand_forest(mode = "regression", mtry = 7, min_n = varying()) %>%
    set_engine("randomForest")
  expr3_exp <- rand_forest(mode = "regression", mtry = 2) %>%
    set_engine("randomForest")

  expr4     <- rand_forest(mode = "regression", mtry = 2) %>%
    set_engine("randomForest", norm.votes = FALSE, sampsize = varying())
  expr4_exp <- rand_forest(mode = "regression", mtry = 2) %>%
    set_engine("randomForest", norm.votes = TRUE, sampsize = 10)

  expr5     <- rand_forest(mode = "regression") %>%
    set_engine("randomForest", norm.votes = varying())
  expr5_exp <- rand_forest(mode = "regression") %>%
    set_engine("randomForest", norm.votes = TRUE)

  expect_equal(update(expr1, mtry = 2), expr1_exp)
  expect_equal(update(expr3, mtry = 2, fresh = TRUE), expr3_exp)
  expect_equal(update(expr4, sampsize = 10, norm.votes = TRUE), expr4_exp)
  expect_equal(update(expr5, norm.votes = TRUE), expr5_exp)

  param_tibb <- tibble::tibble(mtry = 3, trees = 10)
  param_list <- as.list(param_tibb)

  expr4_updated <- update(expr4, param_tibb)
  expect_equal(expr4_updated$args$mtry, 3)
  expect_equal(expr4_updated$args$trees, 10)
  expect_equal(expr4_updated$eng_args$norm.votes, rlang::quo(FALSE))

  expr4_updated_lst <- update(expr4, param_list)
  expect_equal(expr4_updated_lst$args$mtry, 3)
  expect_equal(expr4_updated_lst$args$trees, 10)
  expect_equal(expr4_updated_lst$eng_args$norm.votes, rlang::quo(FALSE))

})

test_that('bad input', {
  expect_error(rand_forest(mode = "time series"))
  expect_error(translate(rand_forest(mode = "classification") %>% set_engine("wat?")))
  expect_error(translate(rand_forest(mode = "classification") %>% set_engine(NULL)))
  expect_error(translate(rand_forest(mode = "classification", ytest = 2)))
})

