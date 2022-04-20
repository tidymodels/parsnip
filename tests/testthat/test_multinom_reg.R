hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------
test_that('updating', {
  expr1 <- multinom_reg() %>% set_engine("glmnet", intercept = TRUE)
  expr2 <- multinom_reg(mixture = tune()) %>% set_engine("glmnet", nlambda = tune())
  expr3 <- multinom_reg(mixture = 0, penalty = tune()) %>% set_engine("glmnet")
  expr4 <- multinom_reg(mixture = 0) %>% set_engine("glmnet", nlambda = 10)
  expr5 <- multinom_reg(mixture = 1) %>% set_engine("glmnet", nlambda = 10)

  param_tibb <- tibble::tibble(mixture = 1/3, penalty = 1)
  param_list <- as.list(param_tibb)

  expect_snapshot(expr1 %>% update(mixture = 0))
  expect_snapshot(expr2 %>% update(nlambda = 10))
  expect_snapshot(expr3 %>% update(mixture = 1, fresh = TRUE))
  expect_snapshot(expr4 %>% update(param_tibb))
  expect_snapshot(expr4 %>% update(param_list))
  expect_snapshot(expr5 %>% update() %>% set_engine("glmnet", nlambda = 10, pmax = 2))
})

test_that('bad input', {
  expect_error(multinom_reg(mode = "regression"))
  expect_error(translate(multinom_reg(penalty = 0.1) %>% set_engine("wat?")))
  expect_error(multinom_reg(penalty = 0.1) %>% set_engine())
  expect_warning(translate(multinom_reg(penalty = 0.1) %>% set_engine("glmnet", x = hpc[,1:3], y = hpc$class)))
})
