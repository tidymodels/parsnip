
test_that('updating', {
  expect_snapshot(
    rand_forest(mode = "regression", mtry = 2) %>%
      set_engine("randomForest", sampsize = 10) %>%
      update(mtry = tune(), sampsize = tune())
  )
})

test_that('bad input', {
  expect_error(translate(rand_forest(mode = "classification") %>% set_engine(NULL)))
  expect_error(rand_forest(mode = "time series"))
  expect_error(translate(rand_forest(mode = "classification") %>% set_engine("wat?")))
  expect_error(translate(rand_forest(mode = "classification", ytest = 2)))
})

test_that("check_args() works", {
  # Here for completeness, no checking is done
  expect_true(TRUE)
}) 