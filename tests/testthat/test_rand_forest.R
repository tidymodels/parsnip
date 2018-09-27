library(testthat)
context("random forest models")
library(parsnip)

test_that('primary arguments', {
  mtry <- rand_forest(mode = "regression", mtry = 4)
  mtry_ranger <- translate(mtry, engine = "ranger")
  mtry_randomForest <- translate(mtry, engine = "randomForest")
  mtry_spark <- translate(mtry, engine = "spark")
  expect_equal(mtry_ranger$method$fit$args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 case.weights = quote(missing_arg()),
                 mtry = 4,
                 num.threads = 1,
                 verbose = FALSE,
                 seed = quote(sample.int(10^5, 1))
               )
  )
  expect_equal(mtry_randomForest$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 mtry = 4
               )
  )
  expect_equal(mtry_spark$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 formula = quote(missing_arg()),
                 type = "regression",
                 feature_subset_strategy = "4",
                 seed = quote(sample.int(10^5, 1))
               )
  )
  trees <- rand_forest(mode = "classification", trees = 1000)
  trees_ranger <- translate(trees, engine = "ranger")
  trees_randomForest <- translate(trees, engine = "randomForest")
  trees_spark <- translate(trees, engine = "spark")
  expect_equal(trees_ranger$method$fit$args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 case.weights = quote(missing_arg()),
                 num.trees = 1000,
                 num.threads = 1,
                 verbose = FALSE,
                 seed = quote(sample.int(10^5, 1)),
                 probability = TRUE
               )
  )
  expect_equal(trees_randomForest$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 ntree = 1000
               )
  )
  expect_equal(trees_spark$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 formula = quote(missing_arg()),
                 type = "classification",
                 num_trees = 1000,
                 seed = quote(sample.int(10^5, 1))
               )
  )

  min_n <- rand_forest(mode = "regression", min_n = 5)
  min_n_ranger <- translate(min_n, engine = "ranger")
  min_n_randomForest <- translate(min_n, engine = "randomForest")
  min_n_spark <- translate(min_n, engine = "spark")
  expect_equal(min_n_ranger$method$fit$args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 case.weights = quote(missing_arg()),
                 min.node.size = 5,
                 num.threads = 1,
                 verbose = FALSE,
                 seed = quote(sample.int(10^5, 1))
               )
  )
  expect_equal(min_n_randomForest$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 nodesize = 5
               )
  )
  expect_equal(min_n_spark$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 formula = quote(missing_arg()),
                 type = "regression",
                 min_instances_per_node = 5,
                 seed = quote(sample.int(10^5, 1))
               )
  )

  mtry_v <- rand_forest(mode = "classification", mtry = varying())
  mtry_v_ranger <- translate(mtry_v, engine = "ranger")
  mtry_v_randomForest <- translate(mtry_v, engine = "randomForest")
  mtry_v_spark <- translate(mtry_v, engine = "spark")
  expect_equal(mtry_v_ranger$method$fit$args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 case.weights = quote(missing_arg()),
                 mtry = varying(),
                 num.threads = 1,
                 verbose = FALSE,
                 seed = quote(sample.int(10^5, 1)),
                 probability = TRUE
               )
  )
  expect_equal(mtry_v_randomForest$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 mtry = varying()
               )
  )
  expect_equal(mtry_v_spark$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 formula = quote(missing_arg()),
                 type = "classification",
                 feature_subset_strategy = varying(),
                 seed = quote(sample.int(10^5, 1))
               )
  )

  trees_v <- rand_forest(mode = "regression", trees = varying())
  trees_v_ranger <- translate(trees_v, engine = "ranger")
  trees_v_randomForest <- translate(trees_v, engine = "randomForest")
  trees_v_spark <- translate(trees_v, engine = "spark")
  expect_equal(trees_v_ranger$method$fit$args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 case.weights = quote(missing_arg()),
                 num.trees = varying(),
                 num.threads = 1,
                 verbose = FALSE,
                 seed = quote(sample.int(10^5, 1))
               )
  )
  expect_equal(trees_v_randomForest$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 ntree = varying()
               )
  )
  expect_equal(trees_v_spark$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 formula = quote(missing_arg()),
                 type = "regression",
                 num_trees = varying(),
                 seed = quote(sample.int(10^5, 1))
               )
  )

  min_n_v <- rand_forest(mode = "classification", min_n = varying())
  min_n_v_ranger <- translate(min_n_v, engine = "ranger")
  min_n_v_randomForest <- translate(min_n_v, engine = "randomForest")
  min_n_v_spark <- translate(min_n_v, engine = "spark")
  expect_equal(min_n_v_ranger$method$fit$args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 case.weights = quote(missing_arg()),
                 min.node.size = varying(),
                 num.threads = 1,
                 verbose = FALSE,
                 seed = quote(sample.int(10^5, 1)),
                 probability = TRUE
               )
  )
  expect_equal(min_n_v_randomForest$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 nodesize = varying()
               )
  )
  expect_equal(min_n_v_spark$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 formula = quote(missing_arg()),
                 type = "classification",
                 min_instances_per_node = varying(),
                 seed = quote(sample.int(10^5, 1))
               )
  )
})

test_that('engine arguments', {
  ranger_imp <- rand_forest(mode = "classification", others = list(importance = "impurity"))
  expect_equal(translate(ranger_imp, engine = "ranger")$method$fit$args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 case.weights = quote(missing_arg()),
                 importance = "impurity",
                 num.threads = 1,
                 verbose = FALSE,
                 seed = quote(sample.int(10^5, 1)),
                 probability = TRUE
               )
  )

  randomForest_votes <- rand_forest(mode = "regression", others = list(norm.votes = FALSE))
  expect_equal(translate(randomForest_votes, engine = "randomForest")$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 norm.votes = FALSE
               )
  )

  spark_gain <- rand_forest(mode = "regression", others = list(min_info_gain = 2))
  expect_equal(translate(spark_gain, engine = "spark")$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 formula = quote(missing_arg()),
                 type = "regression",
                 min_info_gain = 2,
                 seed = quote(sample.int(10^5, 1))
               )
  )

  ranger_samp_frac <- rand_forest(mode = "regression", others = list(sample.fraction = varying()))
  expect_equal(translate(ranger_samp_frac, engine = "ranger")$method$fit$args,
               list(
                 formula = quote(missing_arg()),
                 data = quote(missing_arg()),
                 case.weights = quote(missing_arg()),
                 sample.fraction = varying(),
                 num.threads = 1,
                 verbose = FALSE,
                 seed = quote(sample.int(10^5, 1))
               )
  )


  randomForest_votes_v <- rand_forest(mode = "regression", others = list(norm.votes = FALSE, sampsize = varying()))
  expect_equal(translate(randomForest_votes_v, engine = "randomForest")$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 y = quote(missing_arg()),
                 norm.votes = FALSE,
                 sampsize = varying()
               )
  )

  spark_bins_v <- rand_forest(mode = "regression", others = list(uid = "id label", max_bins = varying()))
  expect_equal(translate(spark_bins_v, engine = "spark")$method$fit$args,
               list(
                 x = quote(missing_arg()),
                 formula = quote(missing_arg()),
                 type = "regression",
                 uid = "id label",
                 max_bins = varying(),
                 seed = quote(sample.int(10^5, 1))
               )
  )
})


test_that('updating', {
  expr1     <- rand_forest(mode = "regression",           others = list(norm.votes = FALSE, sampsize = varying()))
  expr1_exp <- rand_forest(mode = "regression", mtry = 2, others = list(norm.votes = FALSE, sampsize = varying()))

  expr2     <- rand_forest(mode = "regression", mtry = 7, min_n = varying())
  expr2_exp <- rand_forest(mode = "regression", mtry = 7, min_n = varying(), others = list(norm.votes = FALSE))

  expr3     <- rand_forest(mode = "regression", mtry = 7, min_n = varying())
  expr3_exp <- rand_forest(mode = "regression", mtry = 2)

  expr4     <- rand_forest(mode = "regression", mtry = 2, others = list(norm.votes = FALSE, sampsize = varying()))
  expr4_exp <- rand_forest(mode = "regression", mtry = 2, others = list(norm.votes = TRUE, sampsize = varying()))

  expr5     <- rand_forest(mode = "regression", mtry = 2, others = list(norm.votes = FALSE))
  expr5_exp <- rand_forest(mode = "regression", mtry = 2, others = list(norm.votes = TRUE, sampsize = varying()))

  expect_equal(update(expr1, mtry = 2), expr1_exp)
  expect_equal(update(expr2, others = list(norm.votes = FALSE)), expr2_exp)
  expect_equal(update(expr3, mtry = 2, fresh = TRUE), expr3_exp)
  expect_equal(update(expr4, others = list(norm.votes = TRUE)), expr4_exp)
  expect_equal(update(expr5, others = list(norm.votes = TRUE, sampsize = varying())), expr5_exp)

})

test_that('bad input', {
  expect_error(rand_forest(mode = "classification", case.weights = var))
  expect_error(rand_forest(mode = "time series"))
  expect_error(translate(rand_forest(mode = "classification"), engine = "wat?"))
  expect_warning(translate(rand_forest(mode = "classification"), engine = NULL))
  expect_error(translate(rand_forest(mode = "classification", others = list(ytest = 2))))
  expect_error(translate(rand_forest(mode = "regression", formula = y ~ x)))
  expect_error(translate(rand_forest(mode = "classification", others = list(x = x, y = y)), engine = "randomForest"))
  expect_error(translate(rand_forest(mode = "regression", others = list(formula = y ~ x)), engine = ""))
})

