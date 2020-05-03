
set_new_model("nearest_neighbor")

set_model_mode("nearest_neighbor", "classification")
set_model_mode("nearest_neighbor", "regression")

# ------------------------------------------------------------------------------

set_model_engine("nearest_neighbor", "classification", "kknn")
set_model_engine("nearest_neighbor", "regression", "kknn")
set_dependency("nearest_neighbor", "kknn", "kknn")

set_model_arg(
  model = "nearest_neighbor",
  eng = "kknn",
  parsnip = "neighbors",
  original = "ks",
  func = list(pkg = "dials", fun = "neighbors", range = c(1, 15)),
  has_submodel = TRUE
)
set_model_arg(
  model = "nearest_neighbor",
  eng = "kknn",
  parsnip = "weight_func",
  original = "kernel",
  func = list(pkg = "dials", fun = "weight_func"),
  has_submodel = FALSE
)
set_model_arg(
  model = "nearest_neighbor",
  eng = "kknn",
  parsnip = "dist_power",
  original = "distance",
  func = list(pkg = "dials", fun = "dist_power", range = c(1/10, 2)),
  has_submodel = FALSE
)

set_fit(
  model = "nearest_neighbor",
  eng = "kknn",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data"),
    func = c(pkg = "kknn", fun = "train.kknn"),
    defaults = list()
  )
)

set_fit(
  model = "nearest_neighbor",
  eng = "kknn",
  mode = "classification",
  value = list(
    interface = "formula",
    protect = c("formula", "data"),
    func = c(pkg = "kknn", fun = "train.kknn"),
    defaults = list()
  )
)

set_pred(
  model = "nearest_neighbor",
  eng = "kknn",
  mode = "regression",
  type = "numeric",
  value =  list(
    # seems unnecessary here as the predict_numeric catches it based on the
    # model mode
    pre = function(x, object) {
      if (object$fit$response != "continuous") {
        rlang::abort(
          glue::glue("`kknn` model does not appear to use numeric predictions.",
                     " Was the model fit with a continuous response variable?")
        )
      }
      x
    },
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "raw"
      )
  )
)

set_pred(
  model = "nearest_neighbor",
  eng = "kknn",
  mode = "regression",
  type = "raw",
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
  model = "nearest_neighbor",
  eng = "kknn",
  mode = "classification",
  type = "class",
  value = list(
    pre = function(x, object) {
      if (!(object$fit$response %in% c("ordinal", "nominal"))) {
        rlang::abort(
          glue::glue("`kknn` model does not appear to use class predictions.",
                     " Was the model fit with a factor response variable?")
        )
      }
      x
    },
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "raw"
      )
  )
)

set_pred(
  model = "nearest_neighbor",
  eng = "kknn",
  mode = "classification",
  type = "prob",
  value = list(
    pre = function(x, object) {
      if (!(object$fit$response %in% c("ordinal", "nominal"))) {
        rlang::abort(
          glue::glue("`kknn` model does not appear to use class predictions.",
                     " Was the model fit with a factor response variable?")
        )
      }
      x
    },
    post = function(result, object) as_tibble(result),
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
  model = "nearest_neighbor",
  eng = "kknn",
  mode = "classification",
  type = "raw",
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

# ------------------------------------------------------------------------------

set_model_engine("nearest_neighbor", "classification", "FNN")
set_model_engine("nearest_neighbor", "regression", "FNN")
set_dependency("nearest_neighbor", "FNN", "FNN")

set_model_arg(
  model = "nearest_neighbor",
  eng = "FNN",
  parsnip = "neighbors",
  original = "k",
  func = list(pkg = "dials", fun = "neighbors"),
  has_submodel = FALSE
)
set_fit(
  model = "nearest_neighbor",
  eng = "FNN",
  mode = "regression",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    func = c(fun = "fnn_train"),
    defaults = list()
  )
)
set_fit(
  model = "nearest_neighbor",
  eng = "FNN",
  mode = "classification",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    func = c(fun = "fnn_train"),
    defaults = list()
  )
)
set_pred(
  model = "nearest_neighbor",
  eng = "FNN",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "fnn_pred"),
    args = list(
      object = quote(object$fit),
      newdata = quote(new_data)
    )
  )
)
set_pred(
  model = "nearest_neighbor",
  eng = "FNN",
  mode = "regression",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "fnn_pred"),
    args = list(
      object = quote(object$fit),
      newdata = quote(new_data)
    )
  )
)
set_pred(
  model = "nearest_neighbor",
  eng = "FNN",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "fnn_pred"),
    args = list(
      object = quote(object$fit),
      newdata = quote(new_data)
    )
  )
)
set_pred(
  model = "nearest_neighbor",
  eng = "FNN",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = function(result, object) tibble::as_tibble(result),
    func = c(fun = "fnn_pred"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        prob = TRUE
      )
  )
)
set_pred(
  model = "nearest_neighbor",
  eng = "FNN",
  mode = "classification",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "fnn_pred"),
    args = list(
      object = quote(object$fit),
      newdata = quote(new_data))
  )
)
