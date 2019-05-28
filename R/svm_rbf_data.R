set_new_model("svm_rbf")

set_model_mode("svm_rbf", "classification")
set_model_mode("svm_rbf", "regression")

# ------------------------------------------------------------------------------

set_model_engine("svm_rbf", "classification", "kernlab")
set_model_engine("svm_rbf", "regression", "kernlab")
set_dependency("svm_rbf", "kernlab", "kernlab")

set_model_arg(
  model = "svm_rbf",
  eng = "kernlab",
  val = "cost",
  original = "C",
  func = list(pkg = "dials", fun = "cost"),
  submodels = FALSE
)

set_model_arg(
  model = "svm_rbf",
  eng = "kernlab",
  val = "rbf_sigma",
  original = "sigma",
  func = list(pkg = "dials", fun = "rbf_sigma"),
  submodels = FALSE
)

set_model_arg(
  model = "svm_rbf",
  eng = "kernlab",
  val = "margin",
  original = "epsilon",
  func = list(pkg = "dials", fun = "margin"),
  submodels = FALSE
)

set_fit(
  model = "svm_rbf",
  eng = "kernlab",
  mode = "regression",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    func = c(pkg = "kernlab", fun = "ksvm"),
    defaults = list(kernel = "rbfdot")
  )
)

set_fit(
  model = "svm_rbf",
  eng = "kernlab",
  mode = "classification",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    func = c(pkg = "kernlab", fun = "ksvm"),
    defaults = list(kernel = "rbfdot")
  )
)

set_pred(
  model = "svm_rbf",
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
  model = "svm_rbf",
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
  model = "svm_rbf",
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
  model = "svm_rbf",
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
  model = "svm_rbf",
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

