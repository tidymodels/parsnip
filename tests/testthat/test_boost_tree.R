hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

test_that('updating', {
  expect_snapshot(
    boost_tree(trees = 1) %>%
      set_engine("C5.0", noGlobalPruning = TRUE) %>%
      update(trees = tune(), noGlobalPruning = tune())
  )
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

test_that('boost_tree can be fit with 1 predictor if validation is used', {
  spec <- boost_tree(trees = 1) %>%
    set_engine("xgboost", validation = 0.5) %>%
    set_mode("regression")

  expect_no_error(
    fit(spec, mpg ~ disp, data = mtcars)
  )
})
