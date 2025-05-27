# a helper to sanitize the environment of quosures so that
# expect_snapshot results are reproducible
clear_quosure_environment <- function(x) {
  if (rlang::is_quosure(x)) {
    x <- rlang::quo_set_env(x, rlang::empty_env())
  }

  x
}

# a helper to express the idiom of translating, subsetting out the
# generated args, and snapshotting them
translate_args <- function(x) {
  x |>
    translate() |>
    purrr::pluck("method", "fit", "args") |>
    purrr::map(clear_quosure_environment)
}

# in order of `methods("translate")`, testing 1) primary arguments,
# 2) method- and engine-specific arguments, and 3) updates

# translate.boost_tree ---------------------------------------------------------
test_that("arguments (boost_tree)", {
  basic_class <- boost_tree(mode = "classification")
  basic_reg <- boost_tree(mode = "regression")
  trees <- boost_tree(trees = 15, mode = "classification")
  split_num <- boost_tree(min_n = 15, mode = "classification")

  expect_snapshot(translate_args(basic_class |> set_engine("xgboost")))
  expect_snapshot(translate_args(basic_class |> set_engine("C5.0")))
  expect_snapshot(translate_args(basic_class |> set_engine("C5.0", rules = TRUE)))

  expect_snapshot(translate_args(basic_reg |> set_engine("xgboost", print_every_n = 10L)))

  expect_snapshot(translate_args(trees |> set_engine("C5.0")))
  expect_snapshot(translate_args(trees |> set_engine("xgboost")))

  expect_snapshot(translate_args(split_num |> set_engine("C5.0")))
  expect_snapshot(translate_args(split_num |> set_engine("xgboost")))
})

# translate.decision_tree ------------------------------------------------------
test_that("arguments (decision_tree)", {
  basic_class <- decision_tree(mode = "classification")
  basic_reg <- decision_tree(mode = "regression")
  cost_complexity <- decision_tree(cost_complexity = 15, mode = "classification")
  split_num <- decision_tree(min_n = 15, mode = "classification")

  expect_snapshot(translate_args(basic_class |> set_engine("rpart")))
  expect_snapshot(translate_args(basic_class |> set_engine("C5.0")))
  expect_snapshot(translate_args(basic_class |> set_engine("C5.0", rules = TRUE)))

  expect_snapshot(translate_args(basic_reg |> set_engine("rpart", model = TRUE)))

  expect_snapshot(translate_args(cost_complexity |> set_engine("rpart")))

  expect_snapshot(translate_args(split_num |> set_engine("C5.0")))
  expect_snapshot(translate_args(split_num |> set_engine("rpart")))
})


# translate.default ------------------------------------------------------------
test_that("arguments (default)", {
  basic <- null_model(mode = "regression")

  expect_snapshot(translate_args(basic |> set_engine("parsnip")))
  expect_snapshot(translate_args(basic |> set_engine("parsnip", keepxy = FALSE)))
})

# translate.linear_reg ---------------------------------------------------------
test_that("arguments (linear_reg)", {
  basic <- linear_reg()
  mixture <- linear_reg(mixture = 0.128)
  mixture_v <- linear_reg(mixture = tune())
  penalty <- linear_reg(penalty = 1)

  expect_snapshot(translate_args(basic |> set_engine("lm")))
  expect_snapshot(translate_args(basic |> set_engine("lm", model = FALSE)))
  expect_snapshot(translate_args(basic |> set_engine("glm")))
  expect_snapshot(translate_args(basic |> set_engine("glm", family = "quasipoisson")))
  expect_snapshot(translate_args(basic |> set_engine("stan")))
  expect_snapshot(translate_args(basic |> set_engine("stan", chains = 1, iter = 5)))
  expect_snapshot(translate_args(basic |> set_engine("spark")))
  expect_snapshot(translate_args(basic |> set_engine("spark", max_iter = 20)))
  expect_snapshot(translate_args(basic |> set_engine("glmnet")), error = TRUE)

  expect_snapshot(translate_args(mixture |> set_engine("spark")))
  expect_snapshot(translate_args(mixture_v |> set_engine("spark")))
  expect_snapshot(translate_args(mixture |> set_engine("glmnet")), error = TRUE)

  expect_snapshot(translate_args(penalty |> set_engine("glmnet")))
  expect_snapshot(translate_args(penalty |> set_engine("glmnet", nlambda = 10)))
  expect_snapshot(translate_args(penalty |> set_engine("glmnet", path_values = 4:2)))
  expect_snapshot(translate_args(penalty |> set_engine("spark")))
})

# translate.logistic_reg -------------------------------------------------------
test_that("arguments (logistic_reg)", {
  basic <- logistic_reg()
  mixture <- logistic_reg(mixture = 0.128)
  penalty <- logistic_reg(penalty = 1)
  mixture_v <- logistic_reg(mixture = tune())

  expect_snapshot(translate_args(basic |> set_engine("glm")))
  expect_snapshot(translate_args(
    basic |> set_engine("glm", family = binomial(link = "probit"))
  ))

  expect_snapshot(translate_args(basic |> set_engine("glmnet")), error = TRUE)
  expect_snapshot(translate_args(basic |> set_engine("LiblineaR")))
  expect_snapshot(translate_args(basic |> set_engine("LiblineaR", bias = 0)))
  expect_snapshot(translate_args(basic |> set_engine("stan")))
  expect_snapshot(translate_args(basic |> set_engine("stan", chains = 1, iter = 5)))
  expect_snapshot(translate_args(basic |> set_engine("spark")))
  expect_snapshot(translate_args(basic |> set_engine("spark", max_iter = 20)))

  expect_snapshot(translate_args(mixture |> set_engine("glmnet")), error = TRUE)
  expect_snapshot(translate_args(mixture |> set_engine("spark")))

  expect_snapshot(translate_args(penalty |> set_engine("glmnet")))
  expect_snapshot(translate_args(penalty |> set_engine("glmnet", nlambda = 10)))
  expect_snapshot(translate_args(penalty |> set_engine("glmnet", path_values = 4:2)))
  expect_snapshot(translate_args(penalty |> set_engine("LiblineaR")))
  expect_snapshot(translate_args(penalty |> set_engine("spark")))

  expect_snapshot(translate_args(mixture_v |> set_engine("glmnet")), error = TRUE)
  expect_snapshot(translate_args(mixture_v |> set_engine("LiblineaR")))
  expect_snapshot(translate_args(mixture_v |> set_engine("spark")))
})


# translate.mars ---------------------------------------------------------------
test_that("arguments (mars)", {
  basic <- mars(mode = "regression")
  num_terms <- mars(num_terms = 4, mode = "classification")
  prod_degree <- mars(prod_degree = 1, mode = "regression")
  prune_method_v <- mars(prune_method = tune(), mode = "regression")

  expect_snapshot(translate_args(basic |> set_engine("earth")))
  expect_snapshot(translate_args(basic |> set_engine("earth", keepxy = FALSE)))
  expect_snapshot(translate_args(num_terms |> set_engine("earth")))
  expect_snapshot(translate_args(prod_degree |> set_engine("earth")))
  expect_snapshot(translate_args(prune_method_v |> set_engine("earth")))
})

# translate.mlp ----------------------------------------------------------------
test_that("arguments (mlp)", {
  hidden_units <- mlp(mode = "regression", hidden_units = 4)
  no_hidden_units <- mlp(mode = "regression")
  hess <- mlp(mode = "classification")
  all_args <-
    mlp(
      mode = "classification",
      epochs = 2, hidden_units = 4, penalty = 0.0001,
      dropout = 0, activation = "softmax"
    )

  expect_snapshot(translate_args(hidden_units |> set_engine("nnet")))
  expect_snapshot(translate_args(hidden_units |> set_engine("keras")))

  expect_snapshot(translate_args(no_hidden_units |> set_engine("nnet")))
  expect_snapshot(translate_args(no_hidden_units |> set_engine("nnet", abstol = tune())))
  expect_snapshot(translate_args(no_hidden_units |> set_engine("keras", validation_split = 0.2)))

  expect_snapshot(translate_args(hess |> set_engine("nnet", Hess = TRUE)))

  expect_snapshot(translate_args(all_args |> set_engine("nnet")))
  expect_snapshot(translate_args(all_args |> set_engine("keras")))
})


# translate.multinom_reg -------------------------------------------------------
test_that("arguments (multinom_reg)", {
  basic <- multinom_reg()
  mixture <- multinom_reg(penalty = 0.1, mixture = 0.128)
  penalty <- multinom_reg(penalty = 1)
  mixture_v <- multinom_reg(penalty = 0.01, mixture = tune())

  expect_snapshot(translate_args(basic |> set_engine("glmnet")), error = TRUE)
  expect_snapshot(translate_args(mixture |> set_engine("glmnet")))
  expect_snapshot(translate_args(penalty |> set_engine("glmnet")))
  expect_snapshot(translate_args(penalty |> set_engine("glmnet", path_values = 4:2)))
  expect_snapshot(translate_args(penalty |> set_engine("glmnet", nlambda = 10)))
  expect_snapshot(translate_args(mixture_v |> set_engine("glmnet")))
})

# translate.nearest_neighbor ---------------------------------------------------
test_that("arguments (nearest_neighbor)", {
  skip_if_not_installed("kknn")

  basic <- nearest_neighbor(mode = "regression")
  neighbors <- nearest_neighbor(mode = "classification", neighbors = 2)
  weight_func <- nearest_neighbor(mode = "classification", weight_func = "triangular")
  dist_power <- nearest_neighbor(mode = "classification", dist_power = 2)

  expect_snapshot(translate_args(basic |> set_engine("kknn")))
  expect_snapshot(translate_args(neighbors |> set_engine("kknn")))
  expect_snapshot(translate_args(neighbors |> set_engine("kknn", scale = FALSE)))
  expect_snapshot(translate_args(weight_func |> set_engine("kknn")))
  expect_snapshot(translate_args(dist_power |> set_engine("kknn")))
})


# translate.proportional_hazards ------------------------------------------
test_that("arguments (proportional_hazards)", {
  suppressMessages({
    basic <- proportional_hazards(penalty = 0.1) |> set_engine("glmnet")
    basic_incomplete <- proportional_hazards() |> set_engine("glmnet")
  })

  # this is empty because the engines are not defined in parsnip
  expect_snapshot(basic |> translate_args())
  # but we can check for the error if there is no penalty for glmnet
  expect_snapshot(error = TRUE,
    basic_incomplete |> translate_args()
  )
})

# translate.rand_forest --------------------------------------------------------
test_that("arguments (rand_forest)", {
  basic <- rand_forest(mode = "regression")
  mtry <- rand_forest(mode = "regression", mtry = 4)
  trees <- rand_forest(mode = "classification", trees = 1000)
  min_n <- rand_forest(mode = "regression", min_n = 5)

  expect_snapshot(translate_args(basic |> set_engine("randomForest", norm.votes = FALSE)))
  expect_snapshot(translate_args(basic |> set_engine("spark", min_info_gain = 2)))

  expect_snapshot(translate_args(mtry |> set_engine("ranger")))
  expect_snapshot(translate_args(mtry |> set_engine("randomForest")))
  expect_snapshot(translate_args(mtry |> set_engine("spark")))

  expect_snapshot(translate_args(trees |> set_engine("ranger")))
  expect_snapshot(translate_args(trees |> set_engine("ranger", importance = "impurity")))
  expect_snapshot(translate_args(trees |> set_engine("randomForest")))
  expect_snapshot(translate_args(trees |> set_engine("spark")))

  expect_snapshot(translate_args(min_n |> set_engine("ranger")))
  expect_snapshot(translate_args(min_n |> set_engine("randomForest")))
  expect_snapshot(translate_args(min_n |> set_engine("spark")))
})

# translate.surv_reg -----------------------------------------------------------
test_that("arguments (surv_reg)", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  basic <- surv_reg()
  normal <- surv_reg(dist = "lnorm")
  dist_v <- surv_reg(dist = tune())

  expect_snapshot(translate_args(basic  |> set_engine("flexsurv")))
  expect_snapshot(translate_args(basic  |> set_engine("flexsurv", cl = .99)))
  expect_snapshot(translate_args(normal |> set_engine("flexsurv")))
  expect_snapshot(translate_args(dist_v |> set_engine("flexsurv")))
})

# translate.survival_reg -----------------------------------------------------------
test_that("arguments (survival_reg)", {
  suppressMessages({
    basic <- survival_reg()
  })

  # this is empty because the engines are not defined in parsnip
  expect_snapshot(basic |> translate_args())

})

# translate.svm_linear ---------------------------------------------------------
test_that("arguments (svm_linear)", {
  basic <- svm_linear(mode = "regression")

  expect_snapshot(translate_args(basic |> set_engine("LiblineaR")))
  expect_snapshot(translate_args(basic |> set_engine("LiblineaR", type = 12)))
  expect_snapshot(translate_args(basic |> set_engine("kernlab")))
  expect_snapshot(translate_args(basic |> set_engine("kernlab", cross = 10)))
})

# translate.svm_poly -----------------------------------------------------------
test_that("arguments (svm_poly)", {
  basic <- svm_poly(mode = "regression")
  degree <- svm_poly(mode = "regression", degree = 2)
  degree_scale <- svm_poly(mode = "regression", degree = 2, scale_factor = 1.2)

  expect_snapshot(translate_args(basic |> set_engine("kernlab")))
  expect_snapshot(translate_args(basic |> set_engine("kernlab", cross = 10)))
  expect_snapshot(translate_args(degree |> set_engine("kernlab")))
  expect_snapshot(translate_args(degree_scale |> set_engine("kernlab")))
})

# translate.svm_rbf ------------------------------------------------------------
test_that("arguments (svm_rbf)", {
  basic <- svm_rbf(mode = "regression")
  rbf_sigma <- svm_rbf(mode = "regression", rbf_sigma = .2)

  expect_snapshot(translate_args(basic |> set_engine("kernlab")))
  expect_snapshot(translate_args(basic |> set_engine("kernlab", cross = 10)))
  expect_snapshot(translate_args(rbf_sigma |> set_engine("kernlab")))
})

# ------------------------------------------------------------------------------

test_that("translate tuning paramter names", {
  skip_if_not_installed("dials")

  mod <- boost_tree(trees = tune("number of trees"), min_n = tune(), tree_depth = 3)

  expect_snapshot(.model_param_name_key(mod))
  expect_snapshot(.model_param_name_key(mod, as_tibble = FALSE))
  expect_snapshot(.model_param_name_key(linear_reg()))
  expect_snapshot(.model_param_name_key(linear_reg(), as_tibble = FALSE))
  expect_snapshot_error(.model_param_name_key(1))
})

# ------------------------------------------------------------------------------

test_that("get_model_spec helper", {
  mod1 <- get_model_spec("linear_reg", "regression", "lm")

  expect_type(mod1, "list")

  expect_type(mod1$libs, "character")
  expect_length(mod1$libs, 1)
  expect_equal(mod1$libs, "stats")

  expect_type(mod1$fit, "list")
  expect_length(mod1$fit, 4)
  expect_equal(names(mod1$fit), c("interface", "protect", "func", "defaults"))

  expect_type(mod1$pred, "list")
  expect_length(mod1$pred, 4)
  expect_equal(names(mod1$pred), c("numeric", "conf_int", "pred_int", "raw"))

  expect_type(mod1$pred$numeric, "list")
  expect_length(mod1$pred$numeric, 4)
  expect_equal(names(mod1$pred$numeric), c("pre", "post", "func", "args"))

  expect_type(mod1$pred$conf_int, "list")
  expect_length(mod1$pred$conf_int, 4)
  expect_equal(names(mod1$pred$conf_int), c("pre", "post", "func", "args"))

  expect_type(mod1$pred$pred_int, "list")
  expect_length(mod1$pred$pred_int, 4)
  expect_equal(names(mod1$pred$pred_int), c("pre", "post", "func", "args"))

  expect_type(mod1$pred$raw, "list")
  expect_length(mod1$pred$raw, 4)
  expect_equal(names(mod1$pred$raw), c("pre", "post", "func", "args"))
})
