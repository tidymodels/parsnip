set_new_model("decision_tree")

set_model_mode("decision_tree", "classification")
set_model_mode("decision_tree", "regression")
set_model_mode("decision_tree", "censored regression")

# ------------------------------------------------------------------------------

set_model_engine("decision_tree", "classification", "rpart")
set_model_engine("decision_tree", "regression", "rpart")
set_dependency("decision_tree", "rpart", "rpart", mode = "classification")
set_dependency("decision_tree", "rpart", "rpart", mode = "regression")

set_model_arg(
  model = "decision_tree",
  eng = "rpart",
  parsnip = "tree_depth",
  original = "maxdepth",
  func = list(pkg = "dials", fun = "tree_depth"),
  has_submodel = FALSE
)

set_model_arg(
  model = "decision_tree",
  eng = "rpart",
  parsnip = "min_n",
  original = "minsplit",
  func = list(pkg = "dials", fun = "min_n"),
  has_submodel = FALSE
)

set_model_arg(
  model = "decision_tree",
  eng = "rpart",
  parsnip = "cost_complexity",
  original = "cp",
  func = list(pkg = "dials", fun = "cost_complexity"),
  has_submodel = FALSE
)

set_fit(
  model = "decision_tree",
  eng = "rpart",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(pkg = "rpart", fun = "rpart"),
    defaults = list()
  )
)

set_encoding(
  model = "decision_tree",
  eng = "rpart",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_fit(
  model = "decision_tree",
  eng = "rpart",
  mode = "classification",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(pkg = "rpart", fun = "rpart"),
    defaults = list()
  )
)

set_encoding(
  model = "decision_tree",
  eng = "rpart",
  mode = "classification",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "decision_tree",
  eng = "rpart",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(object = quote(object$fit), newdata = quote(new_data))
  )
)

set_pred(
  model = "decision_tree",
  eng = "rpart",
  mode = "regression",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(object = quote(object$fit), newdata = quote(new_data))
  )
)

set_pred(
  model = "decision_tree",
  eng = "rpart",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(pkg = NULL, fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "class"
      )
  )
)

set_pred(
  model = "decision_tree",
  eng = "rpart",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = function(x, object) {
      as_tibble(x)
    },
    func = c(pkg = NULL, fun = "predict"),
    args = list(object = quote(object$fit), newdata = quote(new_data))
  )
)

set_pred(
  model = "decision_tree",
  eng = "rpart",
  mode = "classification",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(object = quote(object$fit), newdata = quote(new_data))
  )
)

# ------------------------------------------------------------------------------

set_model_engine("decision_tree", "classification", "C5.0")
set_dependency("decision_tree", "C5.0", "C50", mode = "classification")

set_model_arg(
  model = "decision_tree",
  eng = "C5.0",
  parsnip = "min_n",
  original = "minCases",
  func = list(pkg = "dials", fun = "min_n"),
  has_submodel = FALSE
)

set_fit(
  model = "decision_tree",
  eng = "C5.0",
  mode = "classification",
  value = list(
    interface = "data.frame",
    protect = c("x", "y", "weights"),
    func = c(pkg = "parsnip", fun = "C5.0_train"),
    defaults = list(trials = 1)
  )
)

set_encoding(
  model = "decision_tree",
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
  model = "decision_tree",
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
  model = "decision_tree",
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
  model = "decision_tree",
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

set_model_engine("decision_tree", "classification", "spark")
set_model_engine("decision_tree", "regression", "spark")
set_dependency("decision_tree", "spark", "sparklyr")

set_model_arg(
  model = "decision_tree",
  eng = "spark",
  parsnip = "tree_depth",
  original = "max_depth",
  func = list(pkg = "dials", fun = "tree_depth"),
  has_submodel = FALSE
)

set_model_arg(
  model = "decision_tree",
  eng = "spark",
  parsnip = "min_n",
  original = "min_instances_per_node",
  func = list(pkg = "dials", fun = "min_n"),
  has_submodel = FALSE
)

set_fit(
  model = "decision_tree",
  eng = "spark",
  mode = "regression",
  value = list(
    interface = "formula",
    data = c(formula = "formula", data = "x"),
    protect = c("x", "formula"),
    func = c(pkg = "sparklyr", fun = "ml_decision_tree_regressor"),
    defaults =
      list(seed = expr(sample.int(10 ^ 5, 1)))
  )
)

set_encoding(
  model = "decision_tree",
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
  model = "decision_tree",
  eng = "spark",
  mode = "classification",
  value = list(
    interface = "formula",
    data = c(formula = "formula", data = "x"),
    protect = c("x", "formula"),
    func = c(pkg = "sparklyr", fun = "ml_decision_tree_classifier"),
    defaults =
      list(seed = expr(sample.int(10 ^ 5, 1)))
  )
)

set_encoding(
  model = "decision_tree",
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
  model = "decision_tree",
  eng = "spark",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = format_spark_num,
    func = c(pkg = "sparklyr", fun = "ml_predict"),
    args = list(object = quote(object$fit), dataset = quote(new_data))
  )
)

set_pred(
  model = "decision_tree",
  eng = "spark",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = format_spark_class,
    func = c(pkg = "sparklyr", fun = "ml_predict"),
    args = list(object = quote(object$fit), dataset = quote(new_data))
  )
)

set_pred(
  model = "decision_tree",
  eng = "spark",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = format_spark_probs,
    func = c(pkg = "sparklyr", fun = "ml_predict"),
    args = list(object = quote(object$fit), dataset = quote(new_data))
  )
)
