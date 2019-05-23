
set_new_model("surv_reg")
set_model_mode("surv_reg", "regression")

# ------------------------------------------------------------------------------

set_model_engine("surv_reg", "regression", "flexsurv")
set_dependency("surv_reg", "flexsurv", "flexsurv")
set_dependency("surv_reg", "flexsurv", "survival")

set_model_arg(
  mod = "surv_reg",
  eng = "flexsurv",
  val = "dist",
  original = "dist",
  func = list(pkg = "dials", fun = "dist"),
  submodels = FALSE
)

set_fit(
  mod = "surv_reg",
  eng = "flexsurv",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(pkg = "flexsurv", fun = "flexsurvreg"),
    defaults = list()
  )
)

set_pred(
  mod = "surv_reg",
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

set_pred(
  mod = "surv_reg",
  eng = "flexsurv",
  mode = "regression",
  type = "quantile",
  value = list(
    pre = NULL,
    post = flexsurv_quant,
    func = c(fun = "summary"),
    args =
      list(
        object = expr(object$fit),
        newdata = expr(new_data),
        type = "quantile",
        quantiles = expr(quantile)
      )
  )
)

# ------------------------------------------------------------------------------

set_model_engine("surv_reg", "regression", "survival")
set_dependency("surv_reg", "survival", "survival")

set_model_arg(
  mod = "surv_reg",
  eng = "survival",
  val = "dist",
  original = "dist",
  func = list(pkg = "dials", fun = "dist"),
  submodels = FALSE
)

set_fit(
  mod = "surv_reg",
  eng = "survival",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(pkg = "survival", fun = "survreg"),
    defaults = list(model = TRUE)
  )
)

set_pred(
  mod = "surv_reg",
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

set_pred(
  mod = "surv_reg",
  eng = "survival",
  mode = "regression",
  type = "quantile",
  value = list(
    pre = NULL,
    post = survreg_quant,
    func = c(fun = "predict"),
    args =
      list(
        object = expr(object$fit),
        newdata = expr(new_data),
        type = "quantile",
        p = expr(quantile)
      )
  )
)
