set_new_model("svm_linear")

set_model_mode("svm_linear", "classification")
set_model_mode("svm_linear", "regression")

# ------------------------------------------------------------------------------

set_model_engine("svm_linear", "classification", "LiblineaR")
set_model_engine("svm_linear", "regression", "LiblineaR")
set_dependency("svm_linear", "LiblineaR", "LiblineaR")

set_model_arg(
  model = "svm_linear",
  eng = "LiblineaR",
  parsnip = "cost",
  original = "C",
  func = list(pkg = "dials", fun = "cost", range = c(-10, 5)),
  has_submodel = FALSE
)

set_model_arg(
  model = "svm_linear",
  eng = "LiblineaR",
  parsnip = "margin",
  original = "svr_eps",
  func = list(pkg = "dials", fun = "svm_margin"),
  has_submodel = FALSE
)

set_fit(
  model = "svm_linear",
  eng = "LiblineaR",
  mode = "regression",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    data = c(x = "data", y = "target"),
    func = c(pkg = "LiblineaR", fun = "LiblineaR"),
    defaults = list(type = 11)
  )
)

set_fit(
  model = "svm_linear",
  eng = "LiblineaR",
  mode = "classification",
  value = list(
    interface = "matrix",
    data = c(x = "data", y = "target"),
    protect = c("x", "y"),
    data = c(x = "data", y = "target"),
    func = c(pkg = "LiblineaR", fun = "LiblineaR"),
    defaults = list(type = 1)
  )
)

set_encoding(
  model = "svm_linear",
  eng = "LiblineaR",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = TRUE
  )
)

set_encoding(
  model = "svm_linear",
  eng = "LiblineaR",
  mode = "classification",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = TRUE
  )
)

set_pred(
  model = "svm_linear",
  eng = "LiblineaR",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = svm_linear_post,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newx = quote(new_data)
      )
  )
)
set_pred(
  model = "svm_linear",
  eng = "LiblineaR",
  mode = "regression",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(
      object = quote(object$fit),
      newx = quote(new_data))
  )
)
set_pred(
  model = "svm_linear",
  eng = "LiblineaR",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = svm_linear_post,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newx = expr(as.matrix(new_data))
      )
  )
)
set_pred(
  model = "svm_linear",
  eng = "LiblineaR",
  mode = "classification",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(
      object = quote(object$fit),
      newx = quote(new_data))
  )
)

# ------------------------------------------------------------------------------

set_model_engine("svm_linear", "classification", "kernlab")
set_model_engine("svm_linear", "regression", "kernlab")
set_dependency("svm_linear", "kernlab", "kernlab")

set_model_arg(
  model = "svm_linear",
  eng = "kernlab",
  parsnip = "cost",
  original = "C",
  func = list(pkg = "dials", fun = "cost", range = c(-10, 5)),
  has_submodel = FALSE
)

set_model_arg(
  model = "svm_linear",
  eng = "kernlab",
  parsnip = "margin",
  original = "epsilon",
  func = list(pkg = "dials", fun = "svm_margin"),
  has_submodel = FALSE
)

set_fit(
  model = "svm_linear",
  eng = "kernlab",
  mode = "regression",
  value = list(
    interface = "formula",
    data = c(formula = "x", data = "data"),
    protect = c("x", "data"),
    func = c(pkg = "kernlab", fun = "ksvm"),
    defaults = list(kernel = "vanilladot")
  )
)

set_fit(
  model = "svm_linear",
  eng = "kernlab",
  mode = "classification",
  value = list(
    interface = "formula",
    data = c(formula = "x", data = "data"),
    protect = c("x", "data"),
    func = c(pkg = "kernlab", fun = "ksvm"),
    defaults = list(kernel = "vanilladot")
  )
)

set_encoding(
  model = "svm_linear",
  eng = "kernlab",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "svm_linear",
  eng = "kernlab",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = svm_reg_linear_post,
    func = c(pkg = "kernlab", fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "response"
      )
  )
)

set_pred(
  model = "svm_linear",
  eng = "kernlab",
  mode = "regression",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(pkg = "kernlab", fun = "predict"),
    args = list(object = quote(object$fit), newdata = quote(new_data))
  )
)

set_encoding(
  model = "svm_linear",
  eng = "kernlab",
  mode = "classification",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "svm_linear",
  eng = "kernlab",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(pkg = "kernlab", fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "response"
      )
  )
)

set_pred(
  model = "svm_linear",
  eng = "kernlab",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = function(result, object) as_tibble(result),
    func = c(pkg = "kernlab", fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "probabilities"
      )
  )
)

set_pred(
  model = "svm_linear",
  eng = "kernlab",
  mode = "classification",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(pkg = "kernlab", fun = "predict"),
    args = list(object = quote(object$fit), newdata = quote(new_data))
  )
)

