# general pattern, for each tunable method:
# define `spec`, run `show_engines()` with only parsnip loaded,
# snapshot test `tunable()` output for each unique engine.
#
# note that, as implemented, parsnip can return `tunable()` information
# for engines that it cannot fit without first loading an extension package.
#
# the specific contents of call_info are just hard-coded tibbles in the
# source, so snapshot testing only for their presence rather than contents.

test_that("tunable.linear_reg()", {
  spec <- linear_reg()
  expect_snapshot(tunable(spec))
  expect_snapshot(tunable(spec %>% set_engine("lm")))
  expect_snapshot(tunable(spec %>% set_engine("glmnet")))
  expect_snapshot(tunable(spec %>% set_engine("brulee")))

  # tests for call_info in tidymodels/extratests
})

test_that("tunable.logistic_reg()", {
  spec <- logistic_reg()
  expect_snapshot(tunable(spec))
  expect_snapshot(tunable(spec %>% set_engine("glm")))
  expect_snapshot(tunable(spec %>% set_engine("glmnet")))
  expect_snapshot(tunable(spec %>% set_engine("brulee")))

  # tests for call_info and additional engines in tidymodels/extratests
})

test_that("tunable.multinom_reg()", {
  spec <- multinom_reg()
  expect_snapshot(tunable(spec))
  expect_snapshot(tunable(spec %>% set_engine("glmnet")))
  expect_snapshot(tunable(spec %>% set_engine("spark")))
  expect_snapshot(tunable(spec %>% set_engine("keras")))
  expect_snapshot(tunable(spec %>% set_engine("nnet")))
  expect_snapshot(tunable(spec %>% set_engine("brulee")))

  # tests for call_info and additional engines in tidymodels/extratests
})

test_that("tunable.boost_tree()", {
  spec <- boost_tree()
  expect_snapshot(tunable(spec))
  expect_snapshot(tunable(spec %>% set_engine("xgboost")))
  expect_snapshot(tunable(spec %>% set_engine("C5.0")))
  expect_snapshot(tunable(spec %>% set_engine("spark")))

  # tests for call_info and additional engines in tidymodels/extratests
})

test_that("tunable.rand_forest()", {
  spec <- rand_forest()
  expect_snapshot(tunable(spec))
  expect_snapshot(tunable(spec %>% set_engine("ranger")))
  expect_snapshot(tunable(spec %>% set_engine("randomForest")))
  expect_snapshot(tunable(spec %>% set_engine("spark")))

  # tests for call_info and additional engines in tidymodels/extratests
})

test_that("tunable.mars()", {
  spec <- mars()
  expect_snapshot(tunable(spec))
  expect_snapshot(tunable(spec %>% set_engine("earth")))

  # tests for call_info and additional engines in tidymodels/extratests
})

test_that("tunable.decision_tree()", {
  spec <- decision_tree()
  expect_snapshot(tunable(spec))
  expect_snapshot(tunable(spec %>% set_engine("rpart")))
  expect_snapshot(tunable(spec %>% set_engine("C5.0")))
  expect_snapshot(tunable(spec %>% set_engine("spark")))

  # tests for call_info and additional engines in tidymodels/extratests
})

test_that("tunable.svm_poly()", {
  spec <- svm_poly()
  expect_snapshot(tunable(spec))
  expect_snapshot(tunable(spec %>% set_engine("kernlab")))

  # tests for call_info and additional engines in tidymodels/extratests
})

test_that("tunable.mlp()", {
  spec <- mlp()
  expect_snapshot(tunable(spec))
  expect_snapshot(tunable(spec %>% set_engine("keras")))
  expect_snapshot(tunable(spec %>% set_engine("nnet")))
  expect_snapshot(tunable(spec %>% set_engine("brulee")))

  # tests for call_info and additional engines in tidymodels/extratests
})


test_that("tunable.survival_reg()", {
  spec <- survival_reg()
  expect_snapshot(tunable(spec))

  # tests for call_info and additional engines in tidymodels/extratests
})
