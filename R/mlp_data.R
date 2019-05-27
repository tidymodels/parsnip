
set_new_model("mlp")

set_model_mode("mlp", "classification")
set_model_mode("mlp", "regression")

# ------------------------------------------------------------------------------

set_model_engine("mlp", "classification", "keras")
set_model_engine("mlp", "regression", "keras")
set_dependency("mlp", "keras", "keras")
set_dependency("mlp", "keras", "magrittr")

set_model_arg(
  mod = "mlp",
  eng = "keras",
  val = "hidden_units",
  original = "hidden_units",
  func = list(pkg = "dials", fun = "hidden_units"),
  submodels = FALSE
)
set_model_arg(
  mod = "mlp",
  eng = "keras",
  val = "penalty",
  original = "penalty",
  func = list(pkg = "dials", fun = "weight_decay"),
  submodels = FALSE
)
set_model_arg(
  mod = "mlp",
  eng = "keras",
  val = "dropout",
  original = "dropout",
  func = list(pkg = "dials", fun = "dropout"),
  submodels = FALSE
)
set_model_arg(
  mod = "mlp",
  eng = "keras",
  val = "epochs",
  original = "epochs",
  func = list(pkg = "dials", fun = "epochs"),
  submodels = FALSE
)
set_model_arg(
  mod = "mlp",
  eng = "keras",
  val = "activation",
  original = "activation",
  func = list(pkg = "dials", fun = "activation"),
  submodels = FALSE
)


set_fit(
  mod = "mlp",
  eng = "keras",
  mode = "regression",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    func = c(pkg = "parsnip", fun = "keras_mlp"),
    defaults = list()
  )
)

set_fit(
  mod = "mlp",
  eng = "keras",
  mode = "classification",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    func = c(pkg = "parsnip", fun = "keras_mlp"),
    defaults = list()
  )
)

set_pred(
  mod = "mlp",
  eng = "keras",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = maybe_multivariate,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        x = quote(as.matrix(new_data))
      )
  )
)

set_pred(
  mod = "mlp",
  eng = "keras",
  mode = "regression",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        x = quote(as.matrix(new_data))
      )
  )

)

set_pred(
  mod = "mlp",
  eng = "keras",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = function(x, object) {
      object$lvl[x + 1]
    },
    func = c(pkg = "keras", fun = "predict_classes"),
    args =
      list(
        object = quote(object$fit),
        x = quote(as.matrix(new_data))
      )
  )
)

set_pred(
  mod = "mlp",
  eng = "keras",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = function(x, object) {
      x <- as_tibble(x)
      colnames(x) <- object$lvl
      x
    },
    func = c(pkg = "keras", fun = "predict_proba"),
    args =
      list(
        object = quote(object$fit),
        x = quote(as.matrix(new_data))
      )
  )
)

set_pred(
  mod = "mlp",
  eng = "keras",
  mode = "classification",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        x = quote(as.matrix(new_data))
      )
  )
)

# ------------------------------------------------------------------------------

set_model_engine("mlp", "classification", "nnet")
set_model_engine("mlp", "regression", "nnet")
set_dependency("mlp", "nnet", "nnet")

set_model_arg(
  mod = "mlp",
  eng = "nnet",
  val = "hidden_units",
  original = "size",
  func = list(pkg = "dials", fun = "hidden_units"),
  submodels = FALSE
)
set_model_arg(
  mod = "mlp",
  eng = "nnet",
  val = "penalty",
  original = "decay",
  func = list(pkg = "dials", fun = "weight_decay"),
  submodels = FALSE
)
set_model_arg(
  mod = "mlp",
  eng = "nnet",
  val = "epochs",
  original = "maxit",
  func = list(pkg = "dials", fun = "epochs"),
  submodels = FALSE
)
set_fit(
  mod = "mlp",
  eng = "nnet",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(pkg = "nnet", fun = "nnet"),
    defaults = list(trace = FALSE)
  )
)

set_fit(
  mod = "mlp",
  eng = "nnet",
  mode = "classification",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(pkg = "nnet", fun = "nnet"),
    defaults = list(trace = FALSE)
  )
)

set_pred(
  mod = "mlp",
  eng = "nnet",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = maybe_multivariate,
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
  mod = "mlp",
  eng = "nnet",
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
  mod = "mlp",
  eng = "nnet",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "class"
      )
  )
)

set_pred(
  mod = "mlp",
  eng = "nnet",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = nnet_softmax,
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
  mod = "mlp",
  eng = "nnet",
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
