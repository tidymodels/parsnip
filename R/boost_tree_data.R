set_new_model("boost_tree")

set_model_mode("boost_tree", "classification")
set_model_mode("boost_tree", "regression")

# ------------------------------------------------------------------------------

set_model_engine("boost_tree", "classification", "xgboost")
set_model_engine("boost_tree", "regression", "xgboost")
set_dependency("boost_tree", "xgboost", "xgboost")

set_model_arg(
  model = "boost_tree",
  eng = "xgboost",
  val = "tree_depth",
  original = "max_depth",
  func = list(pkg = "dials", fun = "tree_depth"),
  submodels = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "xgboost",
  val = "trees",
  original = "nrounds",
  func = list(pkg = "dials", fun = "trees"),
  submodels = TRUE
)
set_model_arg(
  model = "boost_tree",
  eng = "xgboost",
  val = "learn_rate",
  original = "eta",
  func = list(pkg = "dials", fun = "learn_rate"),
  submodels = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "xgboost",
  val = "mtry",
  original = "colsample_bytree",
  func = list(pkg = "dials", fun = "mtry"),
  submodels = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "xgboost",
  val = "min_n",
  original = "min_child_weight",
  func = list(pkg = "dials", fun = "min_n"),
  submodels = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "xgboost",
  val = "loss_reduction",
  original = "gamma",
  func = list(pkg = "dials", fun = "loss_reduction"),
  submodels = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "xgboost",
  val = "sample_size",
  original = "subsample",
  func = list(pkg = "dials", fun = "sample_size"),
  submodels = FALSE
)

set_fit(
  model = "boost_tree",
  eng = "xgboost",
  mode = "regression",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    func = c(pkg = "parsnip", fun = "xgb_train"),
    defaults = list(nthread = 1, verbose = 0)
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
    func = c(fun = "xgb_pred"),
    args = list(object = quote(object$fit), newdata = quote(new_data))
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
    func = c(fun = "xgb_pred"),
    args = list(object = quote(object$fit), newdata = quote(new_data))
  )
)

set_fit(
  model = "boost_tree",
  eng = "xgboost",
  mode = "classification",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    func = c(pkg = "parsnip", fun = "xgb_train"),
    defaults = list(nthread = 1, verbose = 0)
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
        x <- ifelse(x >= 0.5, object$lvl[2], object$lvl[1])
      } else {
        x <- object$lvl[apply(x, 1, which.max)]
      }
      x
    },
    func = c(pkg = NULL, fun = "xgb_pred"),
    args = list(object = quote(object$fit), newdata = quote(new_data))
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
        x <- tibble(v1 = 1 - x, v2 = x)
      } else {
        x <- as_tibble(x)
      }
      colnames(x) <- object$lvl
      x
    },
    func = c(pkg = NULL, fun = "xgb_pred"),
    args = list(object = quote(object$fit), newdata = quote(new_data))
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
    func = c(fun = "xgb_pred"),
    args = list(object = quote(object$fit), newdata = quote(new_data))
  )
)

# ------------------------------------------------------------------------------

set_model_engine("boost_tree", "classification", "C5.0")
set_dependency("boost_tree", "C5.0", "C50")

set_model_arg(
  model = "boost_tree",
  eng = "C5.0",
  val = "trees",
  original = "trials",
  func = list(pkg = "dials", fun = "trees"),
  submodels = TRUE
)
set_model_arg(
  model = "boost_tree",
  eng = "C5.0",
  val = "min_n",
  original = "minCases",
  func = list(pkg = "dials", fun = "min_n"),
  submodels = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "C5.0",
  val = "sample_size",
  original = "sample",
  func = list(pkg = "dials", fun = "sample_size"),
  submodels = FALSE
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
  val = "tree_depth",
  original = "max_depth",
  func = list(pkg = "dials", fun = "tree_depth"),
  submodels = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "spark",
  val = "trees",
  original = "max_iter",
  func = list(pkg = "dials", fun = "trees"),
  submodels = TRUE
)
set_model_arg(
  model = "boost_tree",
  eng = "spark",
  val = "learn_rate",
  original = "step_size",
  func = list(pkg = "dials", fun = "learn_rate"),
  submodels = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "spark",
  val = "mtry",
  original = "feature_subset_strategy",
  func = list(pkg = "dials", fun = "mtry"),
  submodels = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "spark",
  val = "min_n",
  original = "min_instances_per_node",
  func = list(pkg = "dials", fun = "min_n"),
  submodels = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "spark",
  val = "min_info_gain",
  original = "gamma",
  func = list(pkg = "dials", fun = "loss_reduction"),
  submodels = FALSE
)
set_model_arg(
  model = "boost_tree",
  eng = "spark",
  val = "sample_size",
  original = "subsampling_rate",
  func = list(pkg = "dials", fun = "sample_size"),
  submodels = FALSE
)

set_fit(
  model = "boost_tree",
  eng = "spark",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("x", "formula", "type"),
    func = c(pkg = "sparklyr", fun = "ml_gradient_boosted_trees"),
    defaults = list(seed = expr(sample.int(10 ^ 5, 1)))
  )
)

set_fit(
  model = "boost_tree",
  eng = "spark",
  mode = "classification",
  value = list(
    interface = "formula",
    protect = c("x", "formula", "type"),
    func = c(pkg = "sparklyr", fun = "ml_gradient_boosted_trees"),
    defaults = list(seed = expr(sample.int(10 ^ 5, 1)))
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
