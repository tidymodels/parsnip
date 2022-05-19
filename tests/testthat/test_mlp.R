
test_that('updating', {
  expect_snapshot(
    mlp(mode = "classification", hidden_units = 2) %>%
      set_engine("nnet", Hess = FALSE) %>%
      update(hidden_units = tune(), Hess = tune())
  )
})

test_that('bad input', {
  expect_error(mlp(mode = "time series"))
  expect_error(translate(mlp(mode = "classification") %>% set_engine("wat?")))
  expect_warning(translate(mlp(mode = "regression") %>% set_engine("nnet", formula = y ~ x)))
  expect_error(translate(mlp(mode = "classification", x = x, y = y) %>% set_engine("keras")))
  expect_error(translate(mlp(mode = "regression", formula = y ~ x) %>% set_engine()))
})

test_that("nnet_softmax", {
  obj <- mlp(mode = 'classification')
  obj$lvls <- c("a", "b")
  res <- nnet_softmax(matrix(c(.8, .2)), obj)
  expect_equal(names(res), obj$lvls)
  expect_equal(res$b, 1 - res$a)
})

