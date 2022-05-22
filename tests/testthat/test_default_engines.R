test_that('check default engines', {
  expect_equal(boost_tree()$engine, "xgboost")
  expect_equal(decision_tree()$engine, "rpart")
  expect_equal(linear_reg()$engine, "lm")
  expect_equal(logistic_reg()$engine, "glm")
  expect_equal(mars()$engine, "earth")
  expect_equal(mlp()$engine, "nnet")
  expect_equal(multinom_reg()$engine, "nnet")
  expect_equal(nearest_neighbor()$engine, "kknn")

  expect_message(prop_haz <- proportional_hazards()$engine)
  expect_equal(prop_haz, "survival")

  expect_equal(rand_forest()$engine, "ranger")

  expect_message(surv_regr <- survival_reg()$engine)
  expect_equal(surv_regr, "survival")

  expect_equal(svm_linear()$engine, "LiblineaR")
  expect_equal(svm_poly()$engine, "kernlab")
  expect_equal(svm_rbf()$engine, "kernlab")
})
