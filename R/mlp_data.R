
set_new_model("mlp")

set_model_mode("mlp", "classification")
set_model_mode("mlp", "regression")

# ------------------------------------------------------------------------------

set_model_engine("mlp", "classification", "keras")
set_model_engine("mlp", "regression", "keras")
set_dependency("mlp", "keras", "keras")
set_dependency("mlp", "keras", "magrittr")

set_model_arg(
  model = "mlp",
  eng = "keras",
  parsnip = "hidden_units",
  original = "hidden_units",
  func = list(pkg = "dials", fun = "hidden_units"),
  has_submodel = FALSE
)
set_model_arg(
  model = "mlp",
  eng = "keras",
  parsnip = "penalty",
  original = "penalty",
  func = list(pkg = "dials", fun = "penalty"),
  has_submodel = FALSE
)
set_model_arg(
  model = "mlp",
  eng = "keras",
  parsnip = "dropout",
  original = "dropout",
  func = list(pkg = "dials", fun = "dropout"),
  has_submodel = FALSE
)
set_model_arg(
  model = "mlp",
  eng = "keras",
  parsnip = "epochs",
  original = "epochs",
  func = list(pkg = "dials", fun = "epochs"),
  has_submodel = FALSE
)
set_model_arg(
  model = "mlp",
  eng = "keras",
  parsnip = "activation",
  original = "activation",
  func = list(pkg = "dials", fun = "activation"),
  has_submodel = FALSE
)


set_fit(
  model = "mlp",
  eng = "keras",
  mode = "regression",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    func = c(pkg = "parsnip", fun = "keras_mlp"),
    defaults = list()
  )
)

set_encoding(
  model = "mlp",
  eng = "keras",
  mode = "regression",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

set_fit(
  model = "mlp",
  eng = "keras",
  mode = "classification",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    func = c(pkg = "parsnip", fun = "keras_mlp"),
    defaults = list()
  )
)

set_encoding(
  model = "mlp",
  eng = "keras",
  mode = "classification",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "mlp",
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
  model = "mlp",
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
  model = "mlp",
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
  model = "mlp",
  eng = "keras",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = function(x, object) {
      colnames(x) <- object$lvl
      x <- as_tibble(x)
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
  model = "mlp",
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
  model = "mlp",
  eng = "nnet",
  parsnip = "hidden_units",
  original = "size",
  func = list(pkg = "dials", fun = "hidden_units"),
  has_submodel = FALSE
)
set_model_arg(
  model = "mlp",
  eng = "nnet",
  parsnip = "penalty",
  original = "decay",
  func = list(pkg = "dials", fun = "penalty"),
  has_submodel = FALSE
)

set_model_arg(
  model = "mlp",
  eng = "nnet",
  parsnip = "epochs",
  original = "maxit",
  func = list(pkg = "dials", fun = "epochs"),
  has_submodel = FALSE
)

set_fit(
  model = "mlp",
  eng = "nnet",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(pkg = "nnet", fun = "nnet"),
    defaults = list(trace = FALSE)
  )
)

set_encoding(
  model = "mlp",
  eng = "nnet",
  mode = "regression",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

set_fit(
  model = "mlp",
  eng = "nnet",
  mode = "classification",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(pkg = "nnet", fun = "nnet"),
    defaults = list(trace = FALSE)
  )
)

set_encoding(
  model = "mlp",
  eng = "nnet",
  mode = "classification",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "mlp",
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
  model = "mlp",
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
  model = "mlp",
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
  model = "mlp",
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
  model = "mlp",
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

## -----------------------------------------------------------------------------

set_model_engine("mlp", "classification", "brulee")
set_model_engine("mlp", "regression", "brulee")
set_dependency("mlp", "brulee", "brulee")

set_model_arg(
  model = "mlp",
  eng = "brulee",
  parsnip = "hidden_units",
  original = "hidden_units",
  func = list(pkg = "dials", fun = "hidden_units"),
  has_submodel = FALSE
)

set_model_arg(
  model = "mlp",
  eng = "brulee",
  parsnip = "penalty",
  original = "penalty",
  func = list(pkg = "dials", fun = "penalty"),
  has_submodel = FALSE
)

set_model_arg(
  model = "mlp",
  eng = "brulee",
  parsnip = "epochs",
  original = "epochs",
  func = list(pkg = "dials", fun = "epochs"),
  has_submodel = FALSE
)

set_model_arg(
  model = "mlp",
  eng = "brulee",
  parsnip = "dropout",
  original = "dropout",
  func = list(pkg = "dials", fun = "dropout"),
  has_submodel = FALSE
)

set_model_arg(
  model = "mlp",
  eng = "brulee",
  parsnip = "learn_rate",
  original = "learn_rate",
  func = list(pkg = "dials", fun = "learn_rate"),
  has_submodel = FALSE
)

set_model_arg(
  model = "mlp",
  eng = "brulee",
  parsnip = "momentum",
  original = "momentum",
  func = list(pkg = "dials", fun = "momentum"),
  has_submodel = FALSE
)


set_model_arg(
  model = "mlp",
  eng = "brulee",
  parsnip = "stop_iter",
  original = "stop_iter",
  func = list(pkg = "dials", fun = "stop_iter"),
  has_submodel = FALSE
)

set_model_arg(
  model = "mlp",
  eng = "brulee",
  parsnip = "activation",
  original = "activation",
  func = list(pkg = "dials", fun = "activation"),
  has_submodel = FALSE
)


set_fit(
  model = "mlp",
  eng = "brulee",
  mode = "regression",
  value = list(
    interface = "data.frame",
    protect = c("x", "y"),
    func = c(pkg = "brulee", fun = "brulee_mlp"),
    defaults = list()
  )
)

set_encoding(
  model = "mlp",
  eng = "brulee",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_fit(
  model = "mlp",
  eng = "brulee",
  mode = "classification",
  value = list(
    interface = "data.frame",
    protect = c("x", "y"),
    func = c(pkg = "brulee", fun = "brulee_mlp"),
    defaults = list()
  )
)

set_encoding(
  model = "mlp",
  eng = "brulee",
  mode = "classification",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "mlp",
  eng = "brulee",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = reformat_torch_num,
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
  model = "mlp",
  eng = "brulee",
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
  model = "mlp",
  eng = "brulee",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "prob"
      )
  )
)

