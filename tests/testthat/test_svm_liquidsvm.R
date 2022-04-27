hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

test_that('primary arguments', {
  basic <- svm_rbf(mode = "classification")
  basic_liquidSVM <- translate(basic %>% set_engine("liquidSVM"))

  expect_snapshot(basic_liquidSVM$method$fit$args)

  rbf_sigma <-
    svm_rbf(mode = "classification", rbf_sigma = .2) %>%
    set_engine("liquidSVM")
  rbf_sigma_liquidSVM <- translate(rbf_sigma)

  expect_snapshot(rbf_sigma_liquidSVM$method$fit$args)
})

test_that('engine arguments', {

  liquidSVM_scale <-
    svm_rbf() %>%
    set_mode("classification") %>%
    set_engine("liquidSVM", scale = FALSE, predict.prob = TRUE, threads = 2, gpus = 1)

  expect_snapshot(
    translate(liquidSVM_scale, "liquidSVM")$method$fit$args
  )
})


test_that('updating', {

  expr1 <- svm_rbf()  %>% set_engine("liquidSVM", scale = TRUE)
  expr1_exp <- svm_rbf(rbf_sigma = .1) %>% set_engine("liquidSVM", scale = TRUE)

  expr3 <- svm_rbf(rbf_sigma = .2) %>% set_engine("liquidSVM")
  expr3_exp <- svm_rbf(rbf_sigma = .3) %>% set_engine("liquidSVM")

  expect_equal(update(expr1, rbf_sigma = .1), expr1_exp)
  expect_equal(update(expr3, rbf_sigma = .3, fresh = TRUE), expr3_exp)
})

test_that('bad input', {
  expect_error(svm_rbf(mode = "reallyunknown"))
  expect_error(svm_rbf() %>% set_engine( NULL))
})
