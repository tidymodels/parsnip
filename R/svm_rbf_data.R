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
  parsnip = "cost",
  original = "C",
  func = list(pkg = "dials", fun = "cost", range = c(-10, 5)),
  has_submodel = FALSE
)

set_model_arg(
  model = "svm_rbf",
  eng = "kernlab",
  parsnip = "rbf_sigma",
  original = "sigma",
  func = list(pkg = "dials", fun = "rbf_sigma"),
  has_submodel = FALSE
)

set_model_arg(
  model = "svm_rbf",
  eng = "kernlab",
  parsnip = "margin",
  original = "epsilon",
  func = list(pkg = "dials", fun = "svm_margin"),
  has_submodel = FALSE
)

set_fit(
  model = "svm_rbf",
  eng = "kernlab",
  mode = "regression",
  value = list(
    interface = "formula",
    data = c(formula = "x", data = "data"),
    protect = c("x", "data"),
    func = c(pkg = "kernlab", fun = "ksvm"),
    defaults = list(kernel = "rbfdot")
  )
)

set_fit(
  model = "svm_rbf",
  eng = "kernlab",
  mode = "classification",
  value = list(
    interface = "formula",
    data = c(formula = "x", data = "data"),
    protect = c("x", "data"),
    func = c(pkg = "kernlab", fun = "ksvm"),
    defaults = list(kernel = "rbfdot")
  )
)

set_encoding(
  model = "svm_rbf",
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

set_encoding(
  model = "svm_rbf",
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

# ------------------------------------------------------------------------------

set_model_engine("svm_rbf", "classification", "liquidSVM")
set_model_engine("svm_rbf", "regression", "liquidSVM")
set_dependency("svm_rbf", "liquidSVM", "liquidSVM")

set_model_arg(
  model = "svm_rbf",
  eng = "liquidSVM",
  parsnip = "cost",
  original = "lambdas",
  func = list(pkg = "dials", fun = "cost"),
  has_submodel = FALSE
)
set_model_arg(
  model = "svm_rbf",
  eng = "liquidSVM",
  parsnip = "rbf_sigma",
  original = "gammas",
  func = list(pkg = "dials", fun = "rbf_sigma"),
  has_submodel = FALSE
)
set_fit(
  model = "svm_rbf",
  eng = "liquidSVM",
  mode = "regression",
  value = list(
    interface = "data.frame",
    protect = c("x", "y"),
    func = c(pkg = "liquidSVM", fun = "svm"),
    defaults = list(
      folds = 1,
      threads = 0
    )
  )
)

set_encoding(
  model = "svm_rbf",
  eng = "liquidSVM",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_fit(
  model = "svm_rbf",
  eng = "liquidSVM",
  mode = "classification",
  value = list(
    interface = "data.frame",
    protect = c("x", "y"),
    func = c(pkg = "liquidSVM", fun = "svm"),
    defaults = list(
      folds = 1,
      threads = 0
    )
  )
)

set_encoding(
  model = "svm_rbf",
  eng = "liquidSVM",
  mode = "classification",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "svm_rbf",
  eng = "liquidSVM",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
  )
)
set_pred(
  model = "svm_rbf",
  eng = "liquidSVM",
  mode = "regression",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(
      object = quote(object$fit),
      newdata = quote(new_data))
  )
)
set_pred(
  model = "svm_rbf",
  eng = "liquidSVM",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
  )
)
set_pred(
  model = "svm_rbf",
  eng = "liquidSVM",
  mode = "classification",
  type = "prob",
  value = list(
    pre = function(x, object) {
      if (!object$fit$predict.prob) {
        cli::cli_abort(
          c(
            "The model does not appear to use class probabilities.",
            "i" = "Was the model fit with {.code predict.prob = TRUE}?"
          )
        )
      }
      x
    },
    post = function(result, object) {
      res <- tibble::as_tibble(result)
      names(res) <- object$lvl
      res
    },
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        predict.prob = TRUE
      )
  )
)
set_pred(
  model = "svm_rbf",
  eng = "liquidSVM",
  mode = "classification",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(
      object = quote(object$fit),
      newdata = quote(new_data))
  )
)
