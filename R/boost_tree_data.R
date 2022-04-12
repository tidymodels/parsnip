set_new_model("boost_tree")

set_model_mode("boost_tree", "classification")
set_model_mode("boost_tree", "regression")
set_model_mode("boost_tree", "censored regression")

# ------------------------------------------------------------------------------

set_model_engine("boost_tree", "classification", "xgboost")
set_model_engine("boost_tree", "regression", "xgboost")
set_dependency("boost_tree", "xgboost", "xgboost")

set_model_arg(
  model = "boost_tree",
  eng = "xgboost",
  parsnip = "tree_depth",
  original = "max_depth",
  func = list(pkg = "dials", fun = "tree_depth"),
  has_submodel = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "xgboost",
  parsnip = "trees",
  original = "nrounds",
  func = list(pkg = "dials", fun = "trees"),
  has_submodel = TRUE
)
set_model_arg(
  model = "boost_tree",
  eng = "xgboost",
  parsnip = "learn_rate",
  original = "eta",
  func = list(pkg = "dials", fun = "learn_rate"),
  has_submodel = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "xgboost",
  parsnip = "mtry",
  original = "colsample_bynode",
  func = list(pkg = "dials", fun = "mtry"),
  has_submodel = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "xgboost",
  parsnip = "min_n",
  original = "min_child_weight",
  func = list(pkg = "dials", fun = "min_n"),
  has_submodel = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "xgboost",
  parsnip = "loss_reduction",
  original = "gamma",
  func = list(pkg = "dials", fun = "loss_reduction"),
  has_submodel = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "xgboost",
  parsnip = "sample_size",
  original = "subsample",
  func = list(pkg = "dials", fun = "sample_size"),
  has_submodel = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "xgboost",
  parsnip = "stop_iter",
  original = "early_stop",
  func = list(pkg = "dials", fun = "stop_iter"),
  has_submodel = FALSE
)


set_fit(
  model = "boost_tree",
  eng = "xgboost",
  mode = "regression",
  value = list(
    interface = "matrix",
    protect = c("x", "y", "weights"),
    func = c(pkg = "parsnip", fun = "xgb_train"),
    defaults = list(nthread = 1, verbose = 0)
  )
)

set_encoding(
  model = "boost_tree",
  eng = "xgboost",
  mode = "regression",
  options = list(
    predictor_indicators = "one_hot",
    compute_intercept = FALSE,
    remove_intercept = TRUE,
    allow_sparse_x = TRUE
  )
)

set_pred(
  model = "boost_tree",
  eng = "xgboost",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "xgb_predict"),
    args = list(object = quote(object$fit), new_data = quote(new_data))
  )
)

set_pred(
  model = "boost_tree",
  eng = "xgboost",
  mode = "regression",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "xgb_predict"),
    args = list(object = quote(object$fit), new_data = quote(new_data))
  )
)

set_fit(
  model = "boost_tree",
  eng = "xgboost",
  mode = "classification",
  value = list(
    interface = "matrix",
    protect = c("x", "y", "weights"),
    func = c(pkg = "parsnip", fun = "xgb_train"),
    defaults = list(nthread = 1, verbose = 0)
  )
)

set_encoding(
  model = "boost_tree",
  eng = "xgboost",
  mode = "classification",
  options = list(
    predictor_indicators = "one_hot",
    compute_intercept = FALSE,
    remove_intercept = TRUE,
    allow_sparse_x = TRUE
  )
)

set_pred(
  model = "boost_tree",
  eng = "xgboost",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = function(x, object) {
      if (is.vector(x)) {
        event_level <- get_event_level(object$spec)
        if (event_level == "first") {
          x <- ifelse(x >= 0.5, object$lvl[1], object$lvl[2])
        } else {
          x <- ifelse(x >= 0.5, object$lvl[2], object$lvl[1])
        }
      } else {
        x <- object$lvl[apply(x, 1, which.max)]
      }
      x
    },
    func = c(pkg = NULL, fun = "xgb_predict"),
    args = list(object = quote(object$fit), new_data = quote(new_data))
  )
)

set_pred(
  model = "boost_tree",
  eng = "xgboost",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = function(x, object) {
      if (is.vector(x)) {
        event_level <- get_event_level(object$spec)
        if (event_level == "first") {
          x <- tibble(v1 = x, v2 = 1 - x)
        } else {
          x <- tibble(v1 = 1 - x, v2 = x)
        }
      } else {
        x <- as_tibble(x, .name_repair = "minimal")
      }
      colnames(x) <- object$lvl
      x
    },
    func = c(pkg = NULL, fun = "xgb_predict"),
    args = list(object = quote(object$fit), new_data = quote(new_data))
  )
)

set_pred(
  model = "boost_tree",
  eng = "xgboost",
  mode = "classification",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "xgb_predict"),
    args = list(object = quote(object$fit), new_data = quote(new_data))
  )
)

# ------------------------------------------------------------------------------

set_model_engine("boost_tree", "classification", "C5.0")
set_dependency("boost_tree", "C5.0", "C50", mode = "classification")

set_model_arg(
  model = "boost_tree",
  eng = "C5.0",
  parsnip = "trees",
  original = "trials",
  func = list(pkg = "dials", fun = "trees"),
  has_submodel = TRUE
)
set_model_arg(
  model = "boost_tree",
  eng = "C5.0",
  parsnip = "min_n",
  original = "minCases",
  func = list(pkg = "dials", fun = "min_n"),
  has_submodel = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "C5.0",
  parsnip = "sample_size",
  original = "sample",
  func = list(pkg = "dials", fun = "sample_size"),
  has_submodel = FALSE
)

set_fit(
  model = "boost_tree",
  eng = "C5.0",
  mode = "classification",
  value = list(
    interface = "data.frame",
    protect = c("x", "y", "weights"),
    func = c(pkg = "parsnip", fun = "C5.0_train"),
    defaults = list()
  )
)

set_encoding(
  model = "boost_tree",
  eng = "C5.0",
  mode = "classification",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "boost_tree",
  eng = "C5.0",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(object = quote(object$fit), newdata = quote(new_data))
  )
)

set_pred(
  model = "boost_tree",
  eng = "C5.0",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = function(x, object) {
      as_tibble(x)
    },
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "prob"
      )
  )
)

set_pred(
  model = "boost_tree",
  eng = "C5.0",
  mode = "classification",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(object = quote(object$fit),
                newdata = quote(new_data))
  )
)

# ------------------------------------------------------------------------------

set_model_engine("boost_tree", "classification", "spark")
set_model_engine("boost_tree", "regression", "spark")
set_dependency("boost_tree", "spark", "sparklyr")

set_model_arg(
  model = "boost_tree",
  eng = "spark",
  parsnip = "tree_depth",
  original = "max_depth",
  func = list(pkg = "dials", fun = "tree_depth"),
  has_submodel = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "spark",
  parsnip = "trees",
  original = "max_iter",
  func = list(pkg = "dials", fun = "trees"),
  has_submodel = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "spark",
  parsnip = "learn_rate",
  original = "step_size",
  func = list(pkg = "dials", fun = "learn_rate"),
  has_submodel = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "spark",
  parsnip = "mtry",
  original = "feature_subset_strategy",
  func = list(pkg = "dials", fun = "mtry"),
  has_submodel = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "spark",
  parsnip = "min_n",
  original = "min_instances_per_node",
  func = list(pkg = "dials", fun = "min_n"),
  has_submodel = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "spark",
  parsnip = "loss_reduction",
  original = "min_info_gain",
  func = list(pkg = "dials", fun = "loss_reduction"),
  has_submodel = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "spark",
  parsnip = "sample_size",
  original = "subsampling_rate",
  func = list(pkg = "dials", fun = "sample_size"),
  has_submodel = FALSE
)

set_fit(
  model = "boost_tree",
  eng = "spark",
  mode = "regression",
  value = list(
    interface = "formula",
    data = c(formula = "formula", data = "x"),
    protect = c("x", "formula", "type"),
    func = c(pkg = "sparklyr", fun = "ml_gradient_boosted_trees"),
    defaults = list(seed = expr(sample.int(10 ^ 5, 1)))
  )
)

set_encoding(
  model = "boost_tree",
  eng = "spark",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_fit(
  model = "boost_tree",
  eng = "spark",
  mode = "classification",
  value = list(
    interface = "formula",
    data = c(formula = "formula", data = "x"),
    protect = c("x", "formula", "type"),
    func = c(pkg = "sparklyr", fun = "ml_gradient_boosted_trees"),
    defaults = list(seed = expr(sample.int(10 ^ 5, 1)))
  )
)

set_encoding(
  model = "boost_tree",
  eng = "spark",
  mode = "classification",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "boost_tree",
  eng = "spark",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = format_spark_num,
    func = c(pkg = "sparklyr", fun = "ml_predict"),
    args = list(x = quote(object$fit), dataset = quote(new_data))
  )
)

set_pred(
  model = "boost_tree",
  eng = "spark",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = format_spark_class,
    func = c(pkg = "sparklyr", fun = "ml_predict"),
    args = list(x = quote(object$fit), dataset = quote(new_data))
  )
)

set_pred(
  model = "boost_tree",
  eng = "spark",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = format_spark_probs,
    func = c(pkg = "sparklyr", fun = "ml_predict"),
    args = list(x = quote(object$fit), dataset = quote(new_data))
  )
)
