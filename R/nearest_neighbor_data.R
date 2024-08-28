
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

set_encoding(
  model = "nearest_neighbor",
  eng = "kknn",
  mode = "regression",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
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

set_encoding(
  model = "nearest_neighbor",
  eng = "kknn",
  mode = "classification",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
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
        cli::cli_abort(
          c("`kknn` model does not appear to use numeric predictions.",
            "i" = "Was the model fit with a continuous response variable?")
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
        cli::cli_abort(
          c("`kknn` model does not appear to use class predictions.",
            "i" = "Was the model fit with a factor response variable?")
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
        cli::cli_abort(
          c("`kknn` model does not appear to use class predictions.",
            "i" = "Was the model fit with a factor response variable?")
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
