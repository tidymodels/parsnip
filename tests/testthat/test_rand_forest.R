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
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 case.weights = expr(missing_arg()),
                 mtry = new_empty_quosure(4),
                 num.threads = 1,
                 verbose = FALSE,
                 seed = expr(sample.int(10^5, 1))
               )
  )
  expect_equal(mtry_randomForest$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 mtry = new_empty_quosure(4)
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
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
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
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 case.weights = expr(missing_arg()),
                 min.node.size = new_empty_quosure(5),
                 num.threads = 1,
                 verbose = FALSE,
                 seed = expr(sample.int(10^5, 1))
               )
  )
  expect_equal(min_n_randomForest$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 nodesize = new_empty_quosure(5)
               )
  )
  expect_equal(min_n_spark$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 formula = expr(missing_arg()),
                 type = "regression",
                 min_instances_per_node = new_empty_quosure(5),
                 seed = expr(sample.int(10^5, 1))
               )
  )

  mtry_v <- rand_forest(mode = "classification", mtry = varying())
  mtry_v_ranger <- translate(mtry_v %>% set_engine("ranger"))
  mtry_v_randomForest <- translate(mtry_v %>% set_engine("randomForest"))
  mtry_v_spark <- translate(mtry_v %>% set_engine("spark"))
  expect_equal(mtry_v_ranger$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 case.weights = expr(missing_arg()),
                 mtry = new_empty_quosure(varying()),
                 num.threads = 1,
                 verbose = FALSE,
                 seed = expr(sample.int(10^5, 1)),
                 probability = TRUE
               )
  )
  expect_equal(mtry_v_randomForest$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 mtry = new_empty_quosure(varying())
               )
  )
  expect_equal(mtry_v_spark$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 formula = expr(missing_arg()),
                 type = "classification",
                 feature_subset_strategy = new_empty_quosure(varying()),
                 seed = expr(sample.int(10^5, 1))
               )
  )

  trees_v <- rand_forest(mode = "regression", trees = varying())
  trees_v_ranger <- translate(trees_v %>% set_engine("ranger"))
  trees_v_randomForest <- translate(trees_v %>% set_engine("randomForest"))
  trees_v_spark <- translate(trees_v %>% set_engine("spark"))
  expect_equal(trees_v_ranger$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 case.weights = expr(missing_arg()),
                 num.trees = new_empty_quosure(varying()),
                 num.threads = 1,
                 verbose = FALSE,
                 seed = expr(sample.int(10^5, 1))
               )
  )
  expect_equal(trees_v_randomForest$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 ntree = new_empty_quosure(varying())
               )
  )
  expect_equal(trees_v_spark$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 formula = expr(missing_arg()),
                 type = "regression",
                 num_trees = new_empty_quosure(varying()),
                 seed = expr(sample.int(10^5, 1))
               )
  )

  min_n_v <- rand_forest(mode = "classification", min_n = varying())
  min_n_v_ranger <- translate(min_n_v %>% set_engine("ranger"))
  min_n_v_randomForest <- translate(min_n_v %>% set_engine("randomForest"))
  min_n_v_spark <- translate(min_n_v %>% set_engine("spark"))
  expect_equal(min_n_v_ranger$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 case.weights = expr(missing_arg()),
                 min.node.size = new_empty_quosure(varying()),
                 num.threads = 1,
                 verbose = FALSE,
                 seed = expr(sample.int(10^5, 1)),
                 probability = TRUE
               )
  )
  expect_equal(min_n_v_randomForest$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 nodesize = new_empty_quosure(varying())
               )
  )
  expect_equal(min_n_v_spark$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 formula = expr(missing_arg()),
                 type = "classification",
                 min_instances_per_node = new_empty_quosure(varying()),
                 seed = expr(sample.int(10^5, 1))
               )
  )

})

test_that('engine arguments', {
  ranger_imp <- rand_forest(mode = "classification")
  expect_equal(translate(ranger_imp %>% set_engine("ranger", importance = "impurity"))$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
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

  ranger_samp_frac <- rand_forest(mode = "regression")
  expect_equal(
    translate(ranger_samp_frac %>%
                set_engine("ranger", sample.fraction = varying()))$method$fit$args,
    list(
      formula = expr(missing_arg()),
      data = expr(missing_arg()),
      case.weights = expr(missing_arg()),
      sample.fraction = new_empty_quosure(varying()),
      num.threads = 1,
      verbose = FALSE,
      seed = expr(sample.int(10^5, 1))
    )
  )


  randomForest_votes_v <-
    rand_forest(mode = "regression")
  expect_equal(
    translate(randomForest_votes_v %>%
                set_engine("randomForest", norm.votes = FALSE, sampsize = varying()))$method$fit$args,
    list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      norm.votes = new_empty_quosure(FALSE),
      sampsize = new_empty_quosure(varying())
    )
  )

  spark_bins_v <-
    rand_forest(mode = "regression")
  expect_equal(
    translate(spark_bins_v %>%
                set_engine("spark", uid = "id label", max_bins = varying()))$method$fit$args,
    list(
      x = expr(missing_arg()),
      formula = expr(missing_arg()),
      type = "regression",
      uid = new_empty_quosure("id label"),
      max_bins = new_empty_quosure(varying()),
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
    set_engine("randomForest", norm.votes = TRUE, sampsize = varying())

  expr5     <- rand_forest(mode = "regression", mtry = 2) %>%
    set_engine("randomForest", norm.votes = FALSE)
  expr5_exp <- rand_forest(mode = "regression", mtry = 2) %>%
    set_engine("randomForest", norm.votes = TRUE, sampsize = varying())

  expect_equal(update(expr1, mtry = 2), expr1_exp)
  expect_equal(update(expr3, mtry = 2, fresh = TRUE), expr3_exp)

})

test_that('bad input', {
  expect_error(rand_forest(mode = "time series"))
  expect_error(translate(rand_forest(mode = "classification") %>% set_engine("wat?")))
  expect_error(translate(rand_forest(mode = "classification") %>% set_engine(NULL)))
  expect_error(translate(rand_forest(mode = "classification", ytest = 2)))
})

