test_that('basic object classes and print methods', {
  expect_snapshot(print(bag_mars()))

  expect_snapshot(print(bag_tree()))

  expect_snapshot(print(bart()))
  expect_true(inherits(bart(engine = 'dbarts'), 'bart'))

  expect_snapshot(print(boost_tree()))
  expect_true(inherits(boost_tree(engine = 'C5.0'), 'boost_tree'))
  expect_true(inherits(boost_tree(engine = 'spark'), 'boost_tree'))
  expect_true(inherits(boost_tree(engine = 'xgboost'), 'boost_tree'))

  expect_snapshot(print(C5_rules()))

  expect_snapshot(print(cubist_rules()))

  expect_snapshot(print(decision_tree()))
  expect_true(inherits(decision_tree(engine = 'C5.0'), 'decision_tree'))
  expect_true(inherits(decision_tree(engine = 'rpart'), 'decision_tree'))
  expect_true(inherits(decision_tree(engine = 'spark'), 'decision_tree'))

  expect_snapshot(print(discrim_flexible()))

  expect_snapshot(print(discrim_linear()))

  expect_snapshot(print(discrim_quad()))

  expect_snapshot(print(discrim_regularized()))

  expect_snapshot(print(gen_additive_mod()))
  expect_true(inherits(gen_additive_mod(engine = 'mgcv'), 'gen_additive_mod'))

  expect_snapshot(print(linear_reg()))
  expect_true(inherits(linear_reg(engine = 'brulee'), 'linear_reg'))
  expect_true(inherits(linear_reg(engine = 'glm'), 'linear_reg'))
  expect_true(inherits(linear_reg(engine = 'glmnet'), 'linear_reg'))
  expect_true(inherits(linear_reg(engine = 'keras'), 'linear_reg'))
  expect_true(inherits(linear_reg(engine = 'lm'), 'linear_reg'))
  expect_true(inherits(linear_reg(engine = 'spark'), 'linear_reg'))
  expect_true(inherits(linear_reg(engine = 'stan'), 'linear_reg'))

  expect_snapshot(print(logistic_reg()))
  expect_true(inherits(logistic_reg(engine = 'brulee'), 'logistic_reg'))
  expect_true(inherits(logistic_reg(engine = 'glm'), 'logistic_reg'))
  expect_true(inherits(logistic_reg(engine = 'glmnet'), 'logistic_reg'))
  expect_true(inherits(logistic_reg(engine = 'keras'), 'logistic_reg'))
  expect_true(inherits(logistic_reg(engine = 'LiblineaR'), 'logistic_reg'))
  expect_true(inherits(logistic_reg(engine = 'spark'), 'logistic_reg'))
  expect_true(inherits(logistic_reg(engine = 'stan'), 'logistic_reg'))

  expect_snapshot(print(mars()))
  expect_true(inherits(mars(engine = 'earth'), 'mars'))

  expect_snapshot(print(mlp()))
  expect_true(inherits(mlp(engine = 'brulee'), 'mlp'))
  expect_true(inherits(mlp(engine = 'keras'), 'mlp'))
  expect_true(inherits(mlp(engine = 'nnet'), 'mlp'))

  expect_snapshot(print(multinom_reg()))
  expect_true(inherits(multinom_reg(engine = 'brulee'), 'multinom_reg'))
  expect_true(inherits(multinom_reg(engine = 'glmnet'), 'multinom_reg'))
  expect_true(inherits(multinom_reg(engine = 'keras'), 'multinom_reg'))
  expect_true(inherits(multinom_reg(engine = 'nnet'), 'multinom_reg'))
  expect_true(inherits(multinom_reg(engine = 'spark'), 'multinom_reg'))

  expect_snapshot(print(naive_Bayes()))

  expect_snapshot(print(nearest_neighbor()))
  expect_true(inherits(nearest_neighbor(engine = 'kknn'), 'nearest_neighbor'))

  expect_snapshot(print(null_model()))
  expect_true(inherits(null_model(), 'null_model'))

  expect_snapshot(print(pls()))

  expect_snapshot(print(poisson_reg()))

  expect_snapshot(print(proportional_hazards()))

  expect_snapshot(print(rand_forest()))
  expect_true(inherits(rand_forest(engine = 'randomForest'), 'rand_forest'))
  expect_true(inherits(rand_forest(engine = 'ranger'), 'rand_forest'))
  expect_true(inherits(rand_forest(engine = 'spark'), 'rand_forest'))

  expect_snapshot(print(rule_fit()))

  expect_snapshot(print(survival_reg()))

  expect_snapshot(print(svm_linear()))
  expect_true(inherits(svm_linear(engine = 'kernlab'), 'svm_linear'))
  expect_true(inherits(svm_linear(engine = 'LiblineaR'), 'svm_linear'))

  expect_snapshot(print(svm_poly()))
  expect_true(inherits(svm_poly(engine = 'kernlab'), 'svm_poly'))

  expect_snapshot(print(svm_rbf()))
  expect_true(inherits(svm_rbf(engine = 'kernlab'), 'svm_rbf'))
  expect_true(inherits(svm_rbf(engine = 'liquidSVM'), 'svm_rbf'))

})

