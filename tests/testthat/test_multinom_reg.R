hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------
test_that('updating', {
  expect_snapshot(
    multinom_reg(mixture = 0) %>%
      set_engine("glmnet", nlambda = 10) %>%
      update(mixture = tune(), nlambda = tune())
  )
})

test_that('bad input', {
  expect_error(multinom_reg(mode = "regression"))
  expect_error(translate(multinom_reg(penalty = 0.1) %>% set_engine("wat?")))
  expect_error(multinom_reg(penalty = 0.1) %>% set_engine())
  expect_warning(translate(multinom_reg(penalty = 0.1) %>% set_engine("glmnet", x = hpc[,1:3], y = hpc$class)))
})
