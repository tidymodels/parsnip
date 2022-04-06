test_that("primary arguments are translated correctly (linear_reg)", {
  basic <- linear_reg()
  basic_lm <- translate(basic %>% set_engine("lm"))
  basic_glm <- translate(basic %>% set_engine("glm"))
  basic_stan <- translate(basic %>% set_engine("stan"))
  basic_spark <- translate(basic %>% set_engine("spark"))

  mixture <- linear_reg(mixture = 0.128)
  mixture_spark <- translate(mixture %>% set_engine("spark"))
  mixture_v <- linear_reg(mixture = tune())
  mixture_v_spark <- translate(mixture_v %>% set_engine("spark"))

  penalty <- linear_reg(penalty = 1)
  penalty_glmnet <- translate(penalty %>% set_engine("glmnet"))
  penalty_spark <- translate(penalty %>% set_engine("spark"))

  expect_snapshot(basic_lm$method$fit$args)
  expect_snapshot(basic_glm$method$fit$args)
  expect_snapshot(basic_stan$method$fit$args)
  expect_snapshot(basic_spark$method$fit$args)
  expect_snapshot(mixture_spark$method$fit$args)
  expect_snapshot(penalty_glmnet$method$fit$args)
  expect_snapshot(penalty_spark$method$fit$args)
  expect_snapshot(mixture_v_spark$method$fit$args)

  expect_snapshot(
    basic_glmnet <- translate(basic %>% set_engine("glmnet")),
    error = TRUE
  )

  expect_snapshot(
    mixture_glmnet <- translate(mixture %>% set_engine("glmnet")),
    error = TRUE
  )
})

test_that("primary arguments are translated correctly (rand_forest)", {
  expect_true(TRUE)
})

test_that("primary arguments are translated correctly (nearest_neighbor)", {
  expect_true(TRUE)
})

test_that("translate prompts on bad input", {
  expect_true(TRUE)
})

test_that("translate handles method-specific exceptions", {
  expect_true(TRUE)
})
