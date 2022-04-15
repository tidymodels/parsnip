test_that('updating', {
  expr1 <- rand_forest(mode = "regression") %>% set_engine("randomForest", norm.votes = FALSE, sampsize = tune())
  expr2 <- rand_forest(mode = "regression", mtry = 7, min_n = tune()) %>% set_engine("randomForest")
  expr3 <- rand_forest(mode = "regression", mtry = 7, min_n = tune()) %>% set_engine("randomForest")
  expr4 <- rand_forest(mode = "regression", mtry = 2) %>% set_engine("randomForest", norm.votes = FALSE, sampsize = tune())
  expr5 <- rand_forest(mode = "regression") %>% set_engine("randomForest", norm.votes = tune())

  param_tibb <- tibble::tibble(mtry = 3, trees = 10)
  param_list <- as.list(param_tibb)

  expect_snapshot(expr1 %>% update(mtry = 2))
  expect_snapshot(expr3 %>% update(mtry = 2, fresh = TRUE))
  expect_snapshot(expr4 %>% update(sampsize = 10, norm.votes = TRUE))
  expect_snapshot(expr4 %>% update(param_tibb))
  expect_snapshot(expr4 %>% update(param_list))
  expect_snapshot(expr5 %>% update(norm.votes = TRUE))
})

test_that('bad input', {
  expect_error(translate(rand_forest(mode = "classification") %>% set_engine(NULL)))
  expect_error(rand_forest(mode = "time series"))
  expect_error(translate(rand_forest(mode = "classification") %>% set_engine("wat?")))
  expect_error(translate(rand_forest(mode = "classification", ytest = 2)))
})

