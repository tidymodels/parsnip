set_new_model("multinom_reg")

set_model_mode("multinom_reg", "classification")

# ------------------------------------------------------------------------------

set_model_engine("multinom_reg", "classification", "glmnet")
set_dependency("multinom_reg", "glmnet", "glmnet")

set_model_arg(
  mod = "multinom_reg",
  eng = "glmnet",
  val = "penalty",
  original = "lambda",
  func = list(pkg = "dials", fun = "penalty"),
  submodels = TRUE
)

set_model_arg(
  mod = "multinom_reg",
  eng = "glmnet",
  val = "mixture",
  original = "alpha",
  func = list(pkg = "dials", fun = "mixture"),
  submodels = FALSE
)

set_fit(
  mod = "multinom_reg",
  eng = "glmnet",
  mode = "classification",
  value = list(
    interface = "matrix",
    protect = c("x", "y", "weights"),
    func = c(pkg = "glmnet", fun = "glmnet"),
    defaults = list(family = "multinomial")
  )
)


set_pred(
  mod = "multinom_reg",
  eng = "glmnet",
  mode = "classification",
  type = "class",
  value = list(
    pre = check_glmnet_lambda,
    post = organize_multnet_class,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newx = quote(as.matrix(new_data)),
        type = "class",
        s = quote(object$spec$args$penalty)
      )
  )
)

set_pred(
  mod = "multinom_reg",
  eng = "glmnet",
  mode = "classification",
  type = "prob",
  value = list(
    pre = check_glmnet_lambda,
    post = organize_multnet_prob,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newx = quote(as.matrix(new_data)),
        type = "response",
        s = quote(object$spec$args$penalty)
      )
  )
)

set_pred(
  mod = "multinom_reg",
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
  mod = "multinom_reg",
  eng = "glmnet",
  val = "penalty",
  original = "reg_param",
  func = list(pkg = "dials", fun = "penalty"),
  submodels = TRUE
)

set_model_arg(
  mod = "multinom_reg",
  eng = "glmnet",
  val = "elastic_net_param",
  original = "alpha",
  func = list(pkg = "dials", fun = "mixture"),
  submodels = FALSE
)

set_fit(
  mod = "multinom_reg",
  eng = "spark",
  mode = "classification",
  value = list(
    interface = "formula",
    protect = c("x", "formula", "weight_col"),
    func = c(pkg = "sparklyr", fun = "ml_logistic_regression"),
    defaults = list(family = "multinomial")
  )
)

set_pred(
  mod = "multinom_reg",
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
  mod = "multinom_reg",
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
set_model_engine("multinom_reg", "regression", "keras")
set_dependency("multinom_reg", "keras", "keras")
set_dependency("multinom_reg", "keras", "magrittr")

set_model_arg(
  mod = "multinom_reg",
  eng = "keras",
  val = "decay",
  original = "decay",
  func = list(pkg = "dials", fun = "weight_decay"),
  submodels = FALSE
)


set_fit(
  mod = "multinom_reg",
  eng = "keras",
  mode = "classification",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    func = c(pkg = "parsnip", fun = "keras_mlp"),
    defaults = list(hidden_units = 1, act = "linear")
  )
)

set_pred(
  mod = "multinom_reg",
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
      list(object = quote(object$fit),
           x = quote(as.matrix(new_data)))
  )
)

set_pred(
  mod = "multinom_reg",
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
      list(object = quote(object$fit),
           x = quote(as.matrix(new_data)))
  )
)
