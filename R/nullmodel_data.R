set_new_model("null_model")

set_model_mode("null_model", "classification")
set_model_mode("null_model", "regression")

# ------------------------------------------------------------------------------

set_model_engine("null_model", "classification", "parsnip")
set_model_engine("null_model", "regression", "parsnip")
set_dependency("null_model", "parsnip", "parsnip")

set_fit(
  model = "null_model",
  eng = "parsnip",
  mode = "regression",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    func = c(fun = "nullmodel"),
    defaults = list()
  )
)

set_fit(
  model = "null_model",
  eng = "parsnip",
  mode = "classification",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    func = c(fun = "nullmodel"),
    defaults = list()
  )
)

set_pred(
  model = "null_model",
  eng = "parsnip",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "numeric"
      )
  )
)

set_pred(
  model = "null_model",
  eng = "parsnip",
  mode = "regression",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "numeric"
      )
  )
)

set_pred(
  model = "null_model",
  eng = "parsnip",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "class"
      )
  )
)

set_pred(
  model = "null_model",
  eng = "parsnip",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = function(x, object) {
      str(as_tibble(x))
      as_tibble(x)
    },
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "prob"
      )
  )
)

set_pred(
  model = "null_model",
  eng = "parsnip",
  mode = "classification",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "raw"
      )
  )
)

