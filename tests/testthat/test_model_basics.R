test_that('basic object classes and print methods', {
  expect_output(print(bag_mars()), 'Specification')

  expect_output(print(bag_tree()), 'Specification')

  expect_output(print(bart()), 'Specification')
  expect_true(inherits(bart(engine = 'dbarts'), 'bart'))

  expect_output(print(boost_tree()), 'Specification')
  expect_true(inherits(boost_tree(engine = 'C5.0'), 'boost_tree'))
  expect_true(inherits(boost_tree(engine = 'spark'), 'boost_tree'))
  expect_true(inherits(boost_tree(engine = 'xgboost'), 'boost_tree'))

  expect_output(print(C5_rules()), 'Specification')

  expect_output(print(cubist_rules()), 'Specification')

  expect_output(print(decision_tree()), 'Specification')
  expect_true(inherits(decision_tree(engine = 'C5.0'), 'decision_tree'))
  expect_true(inherits(decision_tree(engine = 'rpart'), 'decision_tree'))
  expect_true(inherits(decision_tree(engine = 'spark'), 'decision_tree'))

  expect_output(print(discrim_flexible()), 'Specification')

  expect_output(print(discrim_linear()), 'Specification')

  expect_output(print(discrim_quad()), 'Specification')

  expect_output(print(discrim_regularized()), 'Specification')

  expect_output(print(gen_additive_mod()), 'Specification')
  expect_true(inherits(gen_additive_mod(engine = 'mgcv'), 'gen_additive_mod'))

  expect_output(print(linear_reg()), 'Specification')
  expect_true(inherits(linear_reg(engine = 'brulee'), 'linear_reg'))
  expect_true(inherits(linear_reg(engine = 'glm'), 'linear_reg'))
  expect_true(inherits(linear_reg(engine = 'glmnet'), 'linear_reg'))
  expect_true(inherits(linear_reg(engine = 'keras'), 'linear_reg'))
  expect_true(inherits(linear_reg(engine = 'lm'), 'linear_reg'))
  expect_true(inherits(linear_reg(engine = 'spark'), 'linear_reg'))
  expect_true(inherits(linear_reg(engine = 'stan'), 'linear_reg'))

  expect_output(print(logistic_reg()), 'Specification')
  expect_true(inherits(logistic_reg(engine = 'brulee'), 'logistic_reg'))
  expect_true(inherits(logistic_reg(engine = 'glm'), 'logistic_reg'))
  expect_true(inherits(logistic_reg(engine = 'glmnet'), 'logistic_reg'))
  expect_true(inherits(logistic_reg(engine = 'keras'), 'logistic_reg'))
  expect_true(inherits(logistic_reg(engine = 'LiblineaR'), 'logistic_reg'))
  expect_true(inherits(logistic_reg(engine = 'spark'), 'logistic_reg'))
  expect_true(inherits(logistic_reg(engine = 'stan'), 'logistic_reg'))

  expect_output(print(mars()), 'Specification')
  expect_true(inherits(mars(engine = 'earth'), 'mars'))

  expect_output(print(mlp()), 'Specification')
  expect_true(inherits(mlp(engine = 'brulee'), 'mlp'))
  expect_true(inherits(mlp(engine = 'keras'), 'mlp'))
  expect_true(inherits(mlp(engine = 'nnet'), 'mlp'))

  expect_output(print(multinom_reg()), 'Specification')
  expect_true(inherits(multinom_reg(engine = 'brulee'), 'multinom_reg'))
  expect_true(inherits(multinom_reg(engine = 'glmnet'), 'multinom_reg'))
  expect_true(inherits(multinom_reg(engine = 'keras'), 'multinom_reg'))
  expect_true(inherits(multinom_reg(engine = 'nnet'), 'multinom_reg'))
  expect_true(inherits(multinom_reg(engine = 'spark'), 'multinom_reg'))

  expect_output(print(naive_Bayes()), 'Specification')

  expect_output(print(nearest_neighbor()), 'Specification')
  expect_true(inherits(nearest_neighbor(engine = 'kknn'), 'nearest_neighbor'))

  expect_output(print(null_model()), 'Specification')
  expect_true(inherits(null_model(), 'null_model'))

  expect_output(print(pls()), 'Specification')

  expect_output(print(poisson_reg()), 'Specification')

  expect_output(print(proportional_hazards()), 'Specification')

  expect_output(print(rand_forest()), 'Specification')
  expect_true(inherits(rand_forest(engine = 'randomForest'), 'rand_forest'))
  expect_true(inherits(rand_forest(engine = 'ranger'), 'rand_forest'))
  expect_true(inherits(rand_forest(engine = 'spark'), 'rand_forest'))

  expect_output(print(rule_fit()), 'Specification')

  expect_output(print(survival_reg()), 'Specification')

  expect_output(print(svm_linear()), 'Specification')
  expect_true(inherits(svm_linear(engine = 'kernlab'), 'svm_linear'))
  expect_true(inherits(svm_linear(engine = 'LiblineaR'), 'svm_linear'))

  expect_output(print(svm_poly()), 'Specification')
  expect_true(inherits(svm_poly(engine = 'kernlab'), 'svm_poly'))

  expect_output(print(svm_rbf()), 'Specification')
  expect_true(inherits(svm_rbf(engine = 'kernlab'), 'svm_rbf'))
  expect_true(inherits(svm_rbf(engine = 'liquidSVM'), 'svm_rbf'))

})

