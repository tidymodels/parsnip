hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------
test_that('updating', {
  expr1     <- multinom_reg() %>% set_engine("glmnet", intercept = TRUE)
  expr1_exp <- multinom_reg(mixture = 0) %>% set_engine("glmnet", intercept = TRUE)

  expr2     <- multinom_reg(mixture = tune()) %>% set_engine("glmnet", nlambda = tune())
  expr2_exp <- multinom_reg(mixture = tune()) %>% set_engine("glmnet", nlambda = 10)

  expr3     <- multinom_reg(mixture = 0, penalty = tune()) %>% set_engine("glmnet")
  expr3_exp <- multinom_reg(mixture = 1) %>% set_engine("glmnet")

  expr4     <- multinom_reg(mixture = 0) %>% set_engine("glmnet", nlambda = 10)
  expr4_exp <- multinom_reg(mixture = 0) %>% set_engine("glmnet", nlambda = 10, pmax = 2)

  expr5     <- multinom_reg(mixture = 1) %>% set_engine("glmnet", nlambda = 10)
  expr5_exp <- multinom_reg(mixture = 1) %>% set_engine("glmnet", nlambda = 10, pmax = 2)

  expect_equal(update(expr1, mixture = 0), expr1_exp)
  expect_equal(update(expr2, nlambda = 10), expr2_exp)
  expect_equal(update(expr3, mixture = 1, fresh = TRUE) %>% set_engine("glmnet"), expr3_exp)
  # expect_equal(update(expr4 %>% set_engine("glmnet", pmax = 2)), expr4_exp)
  expect_equal(update(expr5) %>% set_engine("glmnet", nlambda = 10, pmax = 2), expr5_exp)

  param_tibb <- tibble::tibble(mixture = 1/3, penalty = 1)
  param_list <- as.list(param_tibb)

  expr4_updated <- update(expr4, param_tibb)
  expect_equal(expr4_updated$args$mixture, 1/3)
  expect_equal(expr4_updated$args$penalty, 1)
  expect_equal(expr4_updated$eng_args$nlambda, rlang::quo(10))

  expr4_updated_lst <- update(expr4, param_list)
  expect_equal(expr4_updated_lst$args$mixture, 1/3)
  expect_equal(expr4_updated_lst$args$penalty, 1)
  expect_equal(expr4_updated_lst$eng_args$nlambda, rlang::quo(10))

})

test_that('bad input', {
  expect_error(multinom_reg(mode = "regression"))
  expect_error(translate(multinom_reg(penalty = 0.1) %>% set_engine("wat?")))
  expect_error(multinom_reg(penalty = 0.1) %>% set_engine())
  expect_snapshot(translate(multinom_reg(penalty = 0.1) %>% set_engine("glmnet", x = hpc[,1:3], y = hpc$class)))
})
