
set_new_model("surv_reg")
set_model_mode("surv_reg", "regression")

# ------------------------------------------------------------------------------

set_model_engine("surv_reg", mode = "regression", eng = "flexsurv")
set_dependency("surv_reg", eng = "flexsurv", pkg = "flexsurv")
set_dependency("surv_reg", eng = "flexsurv", pkg = "survival")

set_model_arg(
  model = "surv_reg",
  eng = "flexsurv",
  parsnip = "dist",
  original = "dist",
  func = list(pkg = "dials", fun = "surv_dist"),
  has_submodel = FALSE
)

set_fit(
  model = "surv_reg",
  eng = "flexsurv",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(pkg = "flexsurv", fun = "flexsurvreg"),
    defaults = list()
  )
)

set_encoding(
  model = "surv_reg",
  eng = "flexsurv",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "surv_reg",
  eng = "flexsurv",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = flexsurv_mean,
    func = c(fun = "summary"),
    args =
      list(
        object = expr(object$fit),
        newdata = expr(new_data),
        type = "mean"
      )
  )
)

# ------------------------------------------------------------------------------

set_model_engine("surv_reg", mode = "regression", eng = "survival")
set_dependency("surv_reg", eng = "survival", pkg = "survival")

set_model_arg(
  model = "surv_reg",
  eng = "survival",
  parsnip = "dist",
  original = "dist",
  func = list(pkg = "dials", fun = "surv_dist"),
  has_submodel = FALSE
)

set_fit(
  model = "surv_reg",
  eng = "survival",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(pkg = "survival", fun = "survreg"),
    defaults = list(model = TRUE)
  )
)

set_encoding(
  model = "surv_reg",
  eng = "survival",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "surv_reg",
  eng = "survival",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = expr(object$fit),
        newdata = expr(new_data),
        type = "response"
      )
  )
)
