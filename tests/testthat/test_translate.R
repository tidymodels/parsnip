# a helper express the idiom of translating, subsetting out the
# generated args, and snapshotting them
expect_snapshot_args <- function(x, ...) {
  x %>%
    translate() %>%
    purrr::pluck("method", "fit", "args") %>%
    expect_snapshot(...)
}

# in order of `methods("translate")`, testing 1) primary arguments,
# 2) method- and engine-specific arguments, and 3) updates

# translate.boost_tree ---------------------------------------------------------
test_that("primary arguments (boost_tree)", {
  basic <- boost_tree(mode = "classification")
  trees <- boost_tree(trees = 15, mode = "classification")
  split_num <- boost_tree(min_n = 15, mode = "classification")

  expect_snapshot_args(basic %>% set_engine("xgboost"))
  expect_snapshot_args(basic %>% set_engine("C5.0"))

  expect_snapshot_args(trees %>% set_engine("C5.0"))
  expect_snapshot_args(trees %>% set_engine("xgboost"))

  expect_snapshot_args(split_num %>% set_engine("C5.0"))
  expect_snapshot_args(split_num %>% set_engine("xgboost"))
})

# translate.decision_tree ------------------------------------------------------
test_that("primary arguments (decision_tree)", {
  basic <- decision_tree(mode = "classification")
  cost_complexity <- decision_tree(cost_complexity = 15, mode = "classification")
  split_num <- decision_tree(min_n = 15, mode = "classification")

  expect_snapshot_args(basic %>% set_engine("rpart"))
  expect_snapshot_args(basic %>% set_engine("C5.0"))

  expect_snapshot_args(cost_complexity %>% set_engine("rpart"))

  expect_snapshot_args(split_num %>% set_engine("C5.0"))
  expect_snapshot_args(split_num %>% set_engine("rpart"))
})


# translate.default ------------------------------------------------------------
test_that("primary arguments (default)", {
  basic <- null_model(mode = "regression")

  expect_snapshot_args(basic %>% set_engine("parsnip"))
})

# translate.gen_additive_mod ---------------------------------------------------

# translate.linear_reg ---------------------------------------------------------
test_that("primary arguments (linear_reg)", {
  basic <- linear_reg()
  mixture <- linear_reg(mixture = 0.128)
  mixture_v <- linear_reg(mixture = tune())
  penalty <- linear_reg(penalty = 1)

  expect_snapshot_args(basic %>% set_engine("lm"))
  expect_snapshot_args(basic %>% set_engine("glm"))
  expect_snapshot_args(basic %>% set_engine("stan"))
  expect_snapshot_args(basic %>% set_engine("spark"))
  expect_snapshot_args(basic %>% set_engine("glmnet"), error = TRUE)

  expect_snapshot_args(mixture %>% set_engine("spark"))
  expect_snapshot_args(mixture_v %>% set_engine("spark"))
  expect_snapshot_args(mixture %>% set_engine("glmnet"), error = TRUE)

  expect_snapshot_args(penalty %>% set_engine("glmnet"))
  expect_snapshot_args(penalty %>% set_engine("spark"))
})

# translate.logistic_reg -------------------------------------------------------
test_that("primary arguments (logistic_reg)", {
  basic <- logistic_reg()
  mixture <- logistic_reg(mixture = 0.128)
  penalty <- logistic_reg(penalty = 1)
  mixture_v <- logistic_reg(mixture = tune())
  penalty_v <- logistic_reg(penalty = tune())

  expect_snapshot_args(basic %>% set_engine("glm"))
  expect_snapshot_args(basic %>% set_engine("glmnet"), error = TRUE)
  expect_snapshot_args(basic %>% set_engine("LiblineaR"))
  expect_snapshot_args(basic %>% set_engine("stan"))
  expect_snapshot_args(basic %>% set_engine("spark"))

  expect_snapshot_args(mixture %>% set_engine("glmnet"), error = TRUE)
  expect_snapshot_args(mixture %>% set_engine("LiblineaR"))
  expect_snapshot_args(mixture %>% set_engine("spark"))

  expect_snapshot_args(penalty %>% set_engine("glmnet"))
  expect_snapshot_args(penalty %>% set_engine("LiblineaR"))
  expect_snapshot_args(penalty %>% set_engine("spark"))

  expect_snapshot_args(mixture_v %>% set_engine("glmnet"), error = TRUE)
  expect_snapshot_args(mixture_v %>% set_engine("LiblineaR"))
  expect_snapshot_args(mixture_v %>% set_engine("spark"))

  # TODO: shouldn't the following line error on translate?
  expect_snapshot_args(penalty_v %>% set_engine("glmnet"))
  expect_snapshot_args(penalty_v %>% set_engine("LiblineaR"))
  expect_snapshot_args(penalty_v %>% set_engine("spark"))
})


# translate.mars ---------------------------------------------------------------
test_that("primary arguments (mars)", {
  basic <- mars(mode = "regression")
  num_terms <- mars(num_terms = 4, mode = "classification")
  prod_degree <- mars(prod_degree = 1, mode = "regression")
  prune_method_v <- mars(prune_method = tune(), mode = "regression")

  expect_snapshot_args(basic %>% set_engine("earth"))
  expect_snapshot_args(num_terms %>% set_engine("earth"))
  expect_snapshot_args(prod_degree %>% set_engine("earth"))
  expect_snapshot_args(prune_method_v %>% set_engine("earth"))
})

# translate.mlp ----------------------------------------------------------------
test_that("primary arguments (mlp)", {
  hidden_units <- mlp(mode = "regression", hidden_units = 4)
  no_hidden_units <- mlp(mode = "regression")
  all_args <-
    mlp(
      mode = "classification",
      epochs = 2, hidden_units = 4, penalty = 0.0001,
      dropout = 0, activation = "softmax"
    )

  expect_snapshot_args(hidden_units %>% set_engine("nnet"))
  expect_snapshot_args(hidden_units %>% set_engine("keras"))

  expect_snapshot_args(no_hidden_units %>% set_engine("nnet"))

  expect_snapshot_args(all_args %>% set_engine("nnet"))
  expect_snapshot_args(all_args %>% set_engine("keras"))
})


# translate.multinom_reg -------------------------------------------------------
test_that("primary arguments (multinom_reg)", {
  basic <- multinom_reg()
  mixture <- multinom_reg(penalty = 0.1, mixture = 0.128)
  penalty <- multinom_reg(penalty = 1)
  mixture_v <- multinom_reg(penalty = 0.01, mixture = tune())

  expect_snapshot_args(basic %>% set_engine("glmnet"), error = TRUE)
  expect_snapshot_args(mixture %>% set_engine("glmnet"))
  expect_snapshot_args(penalty %>% set_engine("glmnet"))
  expect_snapshot_args(mixture_v %>% set_engine("glmnet"))
})

# translate.nearest_neighbor ---------------------------------------------------
test_that("primary arguments (nearest_neighbor)", {
  basic <- nearest_neighbor(mode = "regression")
  neighbors <- nearest_neighbor(mode = "classification", neighbors = 2)
  weight_func <- nearest_neighbor(mode = "classification", weight_func = "triangular")
  dist_power <- nearest_neighbor(mode = "classification", dist_power = 2)

  expect_snapshot_args(basic %>% set_engine("kknn"))
  expect_snapshot_args(neighbors %>% set_engine("kknn"))
  expect_snapshot_args(weight_func %>% set_engine("kknn"))
  expect_snapshot_args(dist_power %>% set_engine("kknn"))
})

# translate.rand_forest --------------------------------------------------------
test_that("primary arguments (rand_forest)", {
  mtry <- rand_forest(mode = "regression", mtry = 4)
  trees <- rand_forest(mode = "classification", trees = 1000)
  min_n <- rand_forest(mode = "regression", min_n = 5)

  expect_snapshot_args(mtry %>% set_engine("ranger"))
  expect_snapshot_args(mtry %>% set_engine("randomForest"))
  expect_snapshot_args(mtry %>% set_engine("spark"))

  expect_snapshot_args(trees %>% set_engine("ranger"))
  expect_snapshot_args(trees %>% set_engine("randomForest"))
  expect_snapshot_args(trees %>% set_engine("spark"))

  expect_snapshot_args(min_n %>% set_engine("ranger"))
  expect_snapshot_args(min_n %>% set_engine("randomForest"))
  expect_snapshot_args(min_n %>% set_engine("spark"))
})

# translate.surv_reg -----------------------------------------------------------
test_that("primary arguments (surv_reg)", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  basic <- surv_reg()
  normal <- surv_reg(dist = "lnorm")
  dist_v <- surv_reg(dist = tune())

  expect_snapshot_args(basic  %>% set_engine("flexsurv"))
  expect_snapshot_args(normal %>% set_engine("flexsurv"))
  expect_snapshot_args(dist_v %>% set_engine("flexsurv"))
})

# translate.svm_linear ---------------------------------------------------------
test_that("primary arguments (svm_linear)", {
  basic <- svm_linear(mode = "regression")

  expect_snapshot_args(basic %>% set_engine("LiblineaR"))
  expect_snapshot_args(basic %>% set_engine("kernlab"))
})

# translate.svm_poly -----------------------------------------------------------
test_that("primary arguments (svm_poly)", {
  basic <- svm_poly(mode = "regression")
  degree <- svm_poly(mode = "regression", degree = 2)
  degree_scale <- svm_poly(mode = "regression", degree = 2, scale_factor = 1.2)

  expect_snapshot_args(basic %>% set_engine("kernlab"))
  expect_snapshot_args(degree %>% set_engine("kernlab"))
  expect_snapshot_args(degree_scale %>% set_engine("kernlab"))
})

# translate.svm_rbf ------------------------------------------------------------
test_that("primary arguments (svm_rbf)", {
  basic <- svm_rbf(mode = "regression")
  rbf_sigma <- svm_rbf(mode = "regression", rbf_sigma = .2)

  expect_snapshot_args(basic %>% set_engine("kernlab"))
  expect_snapshot_args(rbf_sigma %>% set_engine("kernlab"))
})

