test_that('updating', {
  expr1     <- rand_forest(mode = "regression") %>%
    set_engine("randomForest",  norm.votes = FALSE, sampsize = tune())
  expr1_exp <- rand_forest(mode = "regression", mtry = 2) %>%
    set_engine("randomForest", norm.votes = FALSE, sampsize = tune())

  expr2     <- rand_forest(mode = "regression", mtry = 7, min_n = tune()) %>%
    set_engine("randomForest")
  expr2_exp <- rand_forest(mode = "regression", mtry = 7, min_n = tune() %>%
                             set_engine("randomForest", norm.votes = FALSE))

  expr3     <- rand_forest(mode = "regression", mtry = 7, min_n = tune()) %>%
    set_engine("randomForest")
  expr3_exp <- rand_forest(mode = "regression", mtry = 2) %>%
    set_engine("randomForest")

  expr4     <- rand_forest(mode = "regression", mtry = 2) %>%
    set_engine("randomForest", norm.votes = FALSE, sampsize = tune())
  expr4_exp <- rand_forest(mode = "regression", mtry = 2) %>%
    set_engine("randomForest", norm.votes = TRUE, sampsize = 10)

  expr5     <- rand_forest(mode = "regression") %>%
    set_engine("randomForest", norm.votes = tune())
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
  expect_error(translate(rand_forest(mode = "classification") %>% set_engine(NULL)))
  expect_error(rand_forest(mode = "time series"))
  expect_error(translate(rand_forest(mode = "classification") %>% set_engine("wat?")))
  expect_error(translate(rand_forest(mode = "classification", ytest = 2)))
})

