library(testthat)
library(parsnip)


test_that('primary arguments', {
  mtry <- rand_forest(mode = "classification", mtry = 4)
  mtry_ranger <- finalize(mtry, engine = "ranger")
  mtry_randomForest <- finalize(mtry, engine = "randomForest")
  expect_equal(mtry_ranger$method$fit,
               quote(
                 ranger(
                   formula = formula,
                   data = data,
                   mtry = 4,
                   case.weights = NULL
                 )
               )
  )
  expect_equal(mtry_randomForest$method$fit,
               quote(
                 randomForest(
                   x = x,
                   y = y,
                   mtry = 4
                 )
               )
  )  
  
  trees <- rand_forest(mode = "classification", trees = 1000)
  trees_ranger <- finalize(trees, engine = "ranger")
  trees_randomForest <- finalize(trees, engine = "randomForest")
  expect_equal(trees_ranger$method$fit,
               quote(
                 ranger(
                   formula = formula,
                   data = data,
                   num.trees = 1000,
                   case.weights = NULL
                 )
               )
  )
  expect_equal(trees_randomForest$method$fit,
               quote(
                 randomForest(
                   x = x,
                   y = y,
                   ntree = 1000
                 )
               )
  )    
  
  min_n <- rand_forest(mode = "classification", min_n = 5)
  min_n_ranger <- finalize(min_n, engine = "ranger")
  min_n_randomForest <- finalize(min_n, engine = "randomForest")
  expect_equal(min_n_ranger$method$fit,
               quote(
                 ranger(
                   formula = formula,
                   data = data,
                   min.node.size = 5,
                   case.weights = NULL
                 )
               )
  )
  expect_equal(min_n_randomForest$method$fit,
               quote(
                 randomForest(
                   x = x,
                   y = y,
                   nodesize = 5
                 )
               )
  )      
  
  mtry_v <- rand_forest(mode = "classification", mtry = varying())
  mtry_v_ranger <- finalize(mtry_v, engine = "ranger")
  mtry_v_randomForest <- finalize(mtry_v, engine = "randomForest")
  expect_equal(mtry_v_ranger$method$fit,
               quote(
                 ranger(
                   formula = formula,
                   data = data,
                   mtry = varying(),
                   case.weights = NULL
                 )
               )
  )
  expect_equal(mtry_v_randomForest$method$fit,
               quote(
                 randomForest(
                   x = x,
                   y = y,
                   mtry = varying()
                 )
               )
  )  
  
  trees_v <- rand_forest(mode = "classification", trees = varying())
  trees_v_ranger <- finalize(trees_v, engine = "ranger")
  trees_v_randomForest <- finalize(trees_v, engine = "randomForest")
  expect_equal(trees_v_ranger$method$fit,
               quote(
                 ranger(
                   formula = formula,
                   data = data,
                   num.trees = varying(),
                   case.weights = NULL
                 )
               )
  )
  expect_equal(trees_v_randomForest$method$fit,
               quote(
                 randomForest(
                   x = x,
                   y = y,
                   ntree = varying()
                 )
               )
  )    
  
  min_n_v <- rand_forest(mode = "classification", min_n = varying())
  min_n_v_ranger <- finalize(min_n_v, engine = "ranger")
  min_n_v_randomForest <- finalize(min_n_v, engine = "randomForest")
  expect_equal(min_n_v_ranger$method$fit,
               quote(
                 ranger(
                   formula = formula,
                   data = data,
                   min.node.size = varying(),
                   case.weights = NULL
                 )
               )
  )
  expect_equal(min_n_v_randomForest$method$fit,
               quote(
                 randomForest(
                   x = x,
                   y = y,
                   nodesize = varying()
                 )
               )
  )        
})

test_that('engine arguments', {
  ranger_imp <- rand_forest(mode = "classification", engine_args = list(importance = "impurity"))
  expect_equal(finalize(ranger_imp)$method$fit,
               quote(
                 ranger(
                   formula = formula,
                   data = data,
                   importance = "impurity",
                   case.weights = NULL
                 )
               )
  )
  
  randomForest_votes <- rand_forest(mode = "classification", engine_args = list(norm.votes = FALSE))
  expect_equal(finalize(randomForest_votes, engine = "randomForest")$method$fit,
               quote(
                 randomForest(
                   x = x,
                   y = y,
                   norm.votes = FALSE
                 )
               )
  ) 
  
  ranger_samp_frac <- rand_forest(mode = "classification", engine_args = list(sample.fraction = varying()))
  expect_equal(finalize(ranger_samp_frac)$method$fit,
               quote(
                 ranger(
                   formula = formula,
                   data = data,
                   sample.fraction = varying(),
                   case.weights = NULL
                 )
               )
  ) 
  
  
  randomForest_votes_v <- rand_forest(mode = "classification", engine_args = list(norm.votes = FALSE, sampsize = varying()))
  expect_equal(finalize(randomForest_votes_v, engine = "randomForest")$method$fit,
               quote(
                 randomForest(
                   x = x,
                   y = y,
                   sampsize = varying(),
                   norm.votes = FALSE
                 )
               )
  )   
})


test_that('bad input', {
  expect_error(rand_forest(mode = "classification", case.weights = var))
  expect_error(finalize(rand_forest(mode = "classification", engine_args = list(ytest = 2))))
  
  # is subbing in `var`!
  # rand_forest(mode = "classification", , engine_args = list(case.weights = var))
  
  # should fail for other reasons, adjust check_ellipses?
  # rand_forest(mode = "classification", formula = y ~ x)
  
  # should also fail (unless used in `fit`)
  # finalize(rand_forest(mode = "classification", engine_args = list(formula = y ~ x)))$method$fit
  
})


