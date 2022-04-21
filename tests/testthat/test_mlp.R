
test_that('updating', {
  expr1 <- mlp(mode = "regression") %>% set_engine("nnet", Hess = FALSE, abstol = tune())
  expr2 <- mlp(mode = "regression") %>% set_engine("nnet", Hess = tune())
  expr3 <- mlp(mode = "regression", hidden_units = 7, epochs = tune()) %>% set_engine("keras")
  expr4 <- mlp(mode = "classification", hidden_units = 2) %>% set_engine("nnet", Hess = FALSE, abstol = tune())
  expr5 <- mlp(mode = "classification", hidden_units = 2) %>% set_engine("nnet", Hess = FALSE)

  param_tibb <- tibble::tibble(hidden_units = 3, dropout = .1)
  param_list <- as.list(param_tibb)

  expect_snapshot(expr1 %>% update(hidden_units = 2))
  expect_snapshot(expr2 %>% update(Hess = FALSE))
  expect_snapshot(expr3 %>% update(hidden_units = 2, fresh = TRUE))
  expect_snapshot(expr4 %>% update(abstol = 1e-3))
  expect_snapshot(expr4 %>% update(param_tibb))
  expect_snapshot(expr4 %>% update(param_list))
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

