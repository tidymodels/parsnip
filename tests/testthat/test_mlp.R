test_that('updating', {
  expr1     <- mlp(mode = "regression") %>%
    set_engine("nnet", Hess = FALSE, abstol = tune())
  expr1_exp <- mlp(mode = "regression", hidden_units = 2) %>%
    set_engine("nnet", Hess = FALSE, abstol = tune())

  expr2     <- mlp(mode = "regression") %>% set_engine("nnet", Hess = tune())
  expr2_exp <- mlp(mode = "regression") %>% set_engine("nnet", Hess = FALSE)

  expr3     <- mlp(mode = "regression", hidden_units = 7, epochs = tune()) %>% set_engine("keras")

  expr3_exp <- mlp(mode = "regression", hidden_units = 2) %>% set_engine("keras")

  expr4     <- mlp(mode = "classification", hidden_units = 2) %>% set_engine("nnet", Hess = FALSE, abstol = tune())
  expr4_exp <- mlp(mode = "classification", hidden_units = 2) %>% set_engine("nnet", Hess = FALSE, abstol = 1e-3)

  expr5     <- mlp(mode = "classification", hidden_units = 2) %>% set_engine("nnet", Hess = FALSE)
  expr5_exp <- mlp(mode = "classification", hidden_units = 2) %>% set_engine("nnet", Hess = FALSE, abstol = tune())

  expect_equal(update(expr1, hidden_units = 2), expr1_exp)
  expect_equal(update(expr2, Hess = FALSE), expr2_exp)
  expect_equal(update(expr3, hidden_units = 2, fresh = TRUE), expr3_exp)
  expect_equal(update(expr4, abstol = 1e-3), expr4_exp)

  param_tibb <- tibble::tibble(hidden_units = 3, dropout = .1)
  param_list <- as.list(param_tibb)

  expr4_updated <- update(expr4, param_tibb)
  expect_equal(expr4_updated$args$hidden_units, 3)
  expect_equal(expr4_updated$args$dropout, .1)
  expect_equal(expr4_updated$eng_args$Hess, rlang::quo(FALSE))

  expr4_updated_lst <- update(expr4, param_list)
  expect_equal(expr4_updated_lst$args$hidden_units, 3)
  expect_equal(expr4_updated_lst$args$dropout, .1)
  expect_equal(expr4_updated_lst$eng_args$Hess, rlang::quo(FALSE))

})

test_that('bad input', {
  expect_error(mlp(mode = "time series"))
  expect_error(translate(mlp(mode = "classification") %>% set_engine("wat?")))
  expect_snapshot(translate(mlp(mode = "regression") %>% set_engine("nnet", formula = y ~ x)))
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

