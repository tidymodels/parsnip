set_new_model("tab_pfn")

set_model_mode("tab_pfn", mode = "classification")
set_model_mode("tab_pfn", mode = "regression")

# ------------------------------------------------------------------------------

set_model_engine("tab_pfn", mode = "classification", eng = "tabpfn")
set_dependency("tab_pfn", eng = "tabpfn", pkg = "tabpfn")

set_fit(
  model = "tab_pfn",
  eng = "tabpfn",
  mode = "classification",
  value = list(
    interface = "formula",
    protect = c("formula", "data"),
    func = c(pkg = "tabpfn", fun = "tab_pfn"),
    defaults = list()
  )
)

set_encoding(
  model = "tab_pfn",
  eng = "tabpfn",
  mode = "classification",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "tab_pfn",
  eng = "tabpfn",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(
      object = quote(object$fit),
      new_data = quote(new_data)
    )
  )
)

set_pred(
  model = "tab_pfn",
  eng = "tabpfn",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(
      object = quote(object$fit),
      new_data = quote(new_data)
    )
  )
)

set_model_arg(
  model = "tab_pfn",
  eng = "tabpfn",
  parsnip = "num_estimators",
  original = "num_estimators",
  func = list(pkg = "dials", fun = "num_estimators"),
  has_submodel = FALSE
)

set_model_arg(
  model = "tab_pfn",
  eng = "tabpfn",
  parsnip = "softmax_temperature",
  original = "softmax_temperature",
  func = list(pkg = "dials", fun = "softmax_temperature"),
  has_submodel = FALSE
)

set_model_arg(
  model = "tab_pfn",
  eng = "tabpfn",
  parsnip = "balance_probabilities",
  original = "balance_probabilities",
  func = list(pkg = "dials", fun = "balance_probabilities"),
  has_submodel = FALSE
)

set_model_arg(
  model = "tab_pfn",
  eng = "tabpfn",
  parsnip = "average_before_softmax",
  original = "average_before_softmax",
  func = list(pkg = "dials", fun = "average_before_softmax"),
  has_submodel = FALSE
)

# ------------------------------------------------------------------------------

set_model_engine("tab_pfn", mode = "regression", eng = "tabpfn")
set_dependency("tab_pfn", eng = "tabpfn", pkg = "tabpfn")

set_fit(
  model = "tab_pfn",
  eng = "tabpfn",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data"),
    func = c(pkg = "tabpfn", fun = "tab_pfn"),
    defaults = list()
  )
)

set_encoding(
  model = "tab_pfn",
  eng = "tabpfn",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "tab_pfn",
  eng = "tabpfn",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(
      object = quote(object$fit),
      new_data = quote(new_data)
    )
  )
)
