set_new_model("multinom_reg")

set_model_mode("multinom_reg", "classification")

# ------------------------------------------------------------------------------

set_model_engine("multinom_reg", "classification", "glmnet")
set_dependency("multinom_reg", "glmnet", "glmnet")

set_model_arg(
  model = "multinom_reg",
  eng = "glmnet",
  parsnip = "penalty",
  original = "lambda",
  func = list(pkg = "dials", fun = "penalty"),
  has_submodel = TRUE
)

set_model_arg(
  model = "multinom_reg",
  eng = "glmnet",
  parsnip = "mixture",
  original = "alpha",
  func = list(pkg = "dials", fun = "mixture"),
  has_submodel = FALSE
)

set_fit(
  model = "multinom_reg",
  eng = "glmnet",
  mode = "classification",
  value = list(
    interface = "matrix",
    protect = c("x", "y", "weights"),
    func = c(pkg = "glmnet", fun = "glmnet"),
    defaults = list(family = "multinomial")
  )
)

set_encoding(
  model = "multinom_reg",
  eng = "glmnet",
  mode = "classification",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = TRUE
  )
)

set_pred(
  model = "multinom_reg",
  eng = "glmnet",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = organize_multnet_class,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newx = quote(as.matrix(new_data[, rownames(object$fit$beta[[1]]), drop = FALSE])),
        type = "class",
        s = quote(object$spec$args$penalty)
      )
  )
)

set_pred(
  model = "multinom_reg",
  eng = "glmnet",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = organize_multnet_prob,
    func = c(fun = "predict"),
    args =
      list(
        object = expr(object$fit),
        newx = expr(as.matrix(new_data[, rownames(object$fit$beta[[1]]), drop = FALSE])),
        type = "response",
        s = expr(object$spec$args$penalty)
      )
  )
)

set_pred(
  model = "multinom_reg",
  eng = "glmnet",
  mode = "classification",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newx = quote(as.matrix(new_data))
      )
  )
)

# ------------------------------------------------------------------------------

set_model_engine("multinom_reg", "classification", "spark")
set_dependency("multinom_reg", "spark", "sparklyr")

set_model_arg(
  model = "multinom_reg",
  eng = "spark",
  parsnip = "penalty",
  original = "reg_param",
  func = list(pkg = "dials", fun = "penalty"),
  has_submodel = FALSE
)

set_model_arg(
  model = "multinom_reg",
  eng = "spark",
  parsnip = "mixture",
  original = "elastic_net_param",
  func = list(pkg = "dials", fun = "mixture"),
  has_submodel = FALSE
)

set_fit(
  model = "multinom_reg",
  eng = "spark",
  mode = "classification",
  value = list(
    interface = "formula",
    data = c(formula = "formula", data = "x", weights = "weight_col"),
    protect = c("x", "formula", "weights"),
    func = c(pkg = "sparklyr", fun = "ml_logistic_regression"),
    defaults = list(family = "multinomial")
  )
)

set_encoding(
  model = "multinom_reg",
  eng = "spark",
  mode = "classification",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "multinom_reg",
  eng = "spark",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = format_spark_class,
    func = c(pkg = "sparklyr", fun = "ml_predict"),
    args =
      list(
        x = quote(object$fit),
        dataset = quote(new_data)
      )
  )
)


set_pred(
  model = "multinom_reg",
  eng = "spark",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = format_spark_probs,
    func = c(pkg = "sparklyr", fun = "ml_predict"),
    args =
      list(
        x = quote(object$fit),
        dataset = quote(new_data)
      )
  )
)

# ------------------------------------------------------------------------------

set_model_engine("multinom_reg", "classification", "keras")
set_dependency("multinom_reg", "keras", "keras")
set_dependency("multinom_reg", "keras", "magrittr")

set_model_arg(
  model = "multinom_reg",
  eng = "keras",
  parsnip = "penalty",
  original = "penalty",
  func = list(pkg = "dials", fun = "penalty"),
  has_submodel = FALSE
)


set_fit(
  model = "multinom_reg",
  eng = "keras",
  mode = "classification",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    func = c(pkg = "parsnip", fun = "keras_mlp"),
    defaults = list(hidden_units = 1, act = "linear")
  )
)

set_encoding(
  model = "multinom_reg",
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
  model = "multinom_reg",
  eng = "keras",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(pkg = "parsnip", fun = "keras_predict_classes"),
    args =
      list(object = quote(object),
           x = quote(as.matrix(new_data)))
  )
)

set_pred(
  model = "multinom_reg",
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
    func = c(fun = "predict"),
    args =
      list(object = quote(object$fit),
           x = quote(as.matrix(new_data)))
  )
)


# ------------------------------------------------------------------------------

set_model_engine("multinom_reg", "classification", "nnet")
set_dependency("multinom_reg", "nnet", "nnet")

set_model_arg(
  model = "multinom_reg",
  eng = "nnet",
  parsnip = "penalty",
  original = "decay",
  func = list(pkg = "dials", fun = "penalty"),
  has_submodel = FALSE
)

set_fit(
  model = "multinom_reg",
  eng = "nnet",
  mode = "classification",
  value = list(
    interface = "formula",
    protect = c("formula", "data"),
    func = c(pkg = "nnet", fun = "multinom"),
    defaults = list(trace = FALSE)
  )
)

set_encoding(
  model = "multinom_reg",
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
  model = "multinom_reg",
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
  model = "multinom_reg",
  eng = "nnet",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = organize_nnet_prob,
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
  model = "multinom_reg",
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


# ------------------------------------------------------------------------------


set_model_engine("multinom_reg", "classification", "brulee")
set_dependency("multinom_reg", "brulee", "brulee")


set_model_arg(
  model = "multinom_reg",
  eng = "brulee",
  parsnip = "penalty",
  original = "penalty",
  func = list(pkg = "dials", fun = "penalty"),
  has_submodel = FALSE
)

set_model_arg(
  model = "multinom_reg",
  eng = "brulee",
  parsnip = "mixture",
  original = "mixture",
  func = list(pkg = "dials", fun = "mixture"),
  has_submodel = FALSE
)

set_fit(
  model = "multinom_reg",
  eng = "brulee",
  mode = "classification",
  value = list(
    interface = "data.frame",
    protect = c("x", "y"),
    func = c(pkg = "brulee", fun = "brulee_multinomial_reg"),
    defaults = list()
  )
)

set_encoding(
  model = "multinom_reg",
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
  model = "multinom_reg",
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
  model = "multinom_reg",
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
