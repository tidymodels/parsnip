set_new_model("svm_poly")

set_model_mode("svm_poly", "classification")
set_model_mode("svm_poly", "regression")

# ------------------------------------------------------------------------------

set_model_engine("svm_poly", "classification", "kernlab")
set_model_engine("svm_poly", "regression", "kernlab")
set_dependency("svm_poly", "kernlab", "kernlab")

set_model_arg(
  model = "svm_poly",
  eng = "kernlab",
  parsnip = "cost",
  original = "C",
  func = list(pkg = "dials", fun = "cost", range = c(-10, 5)),
  has_submodel = FALSE
)

set_model_arg(
  model = "svm_poly",
  eng = "kernlab",
  parsnip = "degree",
  original = "degree",
  func = list(pkg = "dials", fun = "degree"),
  has_submodel = FALSE
)

set_model_arg(
  model = "svm_poly",
  eng = "kernlab",
  parsnip = "scale_factor",
  original = "scale",
  func = list(pkg = "dials", fun = "scale_factor"),
  has_submodel = FALSE
)
set_model_arg(
  model = "svm_poly",
  eng = "kernlab",
  parsnip = "margin",
  original = "epsilon",
  func = list(pkg = "dials", fun = "svm_margin"),
  has_submodel = FALSE
)

set_fit(
  model = "svm_poly",
  eng = "kernlab",
  mode = "regression",
  value = list(
    interface = "formula",
    data = c(formula = "x", data = "data"),
    protect = c("x", "data"),
    func = c(pkg = "kernlab", fun = "ksvm"),
    defaults = list(kernel = "polydot")
  )
)

set_encoding(
  model = "svm_poly",
  eng = "kernlab",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_fit(
  model = "svm_poly",
  eng = "kernlab",
  mode = "classification",
  value = list(
    interface = "formula",
    data = c(formula = "x", data = "data"),
    protect = c("x", "data"),
    func = c(pkg = "kernlab", fun = "ksvm"),
    defaults = list(kernel = "polydot")
  )
)

set_encoding(
  model = "svm_poly",
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
  model = "svm_poly",
  eng = "kernlab",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = svm_reg_post,
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
  model = "svm_poly",
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

set_pred(
  model = "svm_poly",
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
  model = "svm_poly",
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
  model = "svm_poly",
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

