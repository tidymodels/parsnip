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

# translate.decision_tree ------------------------------------------------------

# translate.default ------------------------------------------------------------

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

# translate.mars ---------------------------------------------------------------

# translate.mlp ----------------------------------------------------------------

# translate.multinom_reg -------------------------------------------------------

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

# translate.poisson_reg --------------------------------------------------------

# translate.proportional_hazards -----------------------------------------------

# translate.rand_forest
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

# translate.survival_reg -------------------------------------------------------

# translate.svm_linear ---------------------------------------------------------

# translate.svm_poly -----------------------------------------------------------

# translate.svm_rbf ------------------------------------------------------------






