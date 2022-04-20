hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

test_that('updating', {
  expr1 <- boost_tree() %>% set_engine("xgboost", verbose = 0)
  expr2 <- boost_tree(trees = tune()) %>% set_engine("C5.0", bands = tune())
  expr3 <- boost_tree(trees = 1, sample_size = tune())
  expr4 <- boost_tree() %>% set_engine("C5.0", noGlobalPruning = tune())

  param_tibb <- tibble::tibble(trees = 7, mtry = 1)
  param_list <- as.list(param_tibb)

  expect_snapshot(expr1 %>% update(trees = 10))
  expect_snapshot(expr1 %>% update(param_tibb))
  expect_snapshot(expr1 %>% update(param_list))
  expect_snapshot(expr2 %>% update(bands = 10))
  expect_snapshot(expr3 %>% update(trees = 1, fresh = TRUE))
  expect_snapshot(expr4 %>% update(noGlobalPruning = TRUE))
})

test_that('bad input', {
  expect_error(boost_tree(mode = "bogus"))
  expect_error({
    bt <- boost_tree(trees = -1) %>% set_engine("xgboost")
    fit(bt, class ~ ., hpc)
  })
  expect_error({
    bt <- boost_tree(min_n = -10)  %>% set_engine("xgboost")
    fit(bt, class ~ ., hpc)
  })
  expect_message(translate(boost_tree(mode = "classification"), engine = NULL))
  expect_error(translate(boost_tree(formula = y ~ x)))
})


## -----------------------------------------------------------------------------

test_that('argument checks for data dimensions', {

  spec <-
    boost_tree(mtry = 1000, min_n = 1000, trees = 5) %>%
    set_engine("spark") %>%
    set_mode("classification")

  args <- translate(spec)$method$fit$args
  expect_equal(args$min_instances_per_node, expr(min_rows(1000, x)))
})

