set_new_model("linear_reg")

set_model_mode("linear_reg", "regression")
set_model_mode("linear_reg", "quantile regression")

# ------------------------------------------------------------------------------

set_model_engine("linear_reg", "regression", "lm")
set_dependency("linear_reg", "lm", "stats")

set_fit(
  model = "linear_reg",
  eng = "lm",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(pkg = "stats", fun = "lm"),
    defaults = list()
  )
)

set_encoding(
  model = "linear_reg",
  eng = "lm",
  mode = "regression",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "linear_reg",
  eng = "lm",
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
        type = "response",
        rankdeficient = "simple"
      )
  )
)

set_pred(
  model = "linear_reg",
  eng = "lm",
  mode = "regression",
  type = "conf_int",
  value = list(
    pre = NULL,
    post = function(results, object) {
      tibble::as_tibble(results) |>
        dplyr::select(-fit) |>
        setNames(c(".pred_lower", ".pred_upper"))
    },
    func = c(fun = "predict"),
    args =
      list(
        object = expr(object$fit),
        newdata = expr(new_data),
        interval = "confidence",
        level = expr(level),
        type = "response"
      )
  )
)
set_pred(
  model = "linear_reg",
  eng = "lm",
  mode = "regression",
  type = "pred_int",
  value = list(
    pre = NULL,
    post = function(results, object) {
      tibble::as_tibble(results) |>
        dplyr::select(-fit) |>
        setNames(c(".pred_lower", ".pred_upper"))
    },
    func = c(fun = "predict"),
    args =
      list(
        object = expr(object$fit),
        newdata = expr(new_data),
        interval = "prediction",
        level = expr(level),
        type = "response"
      )
  )
)

set_pred(
  model = "linear_reg",
  eng = "lm",
  mode = "regression",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(object = expr(object$fit), newdata = expr(new_data))
  )
)

# ------------------------------------------------------------------------------

set_model_engine("linear_reg", "regression", "glm")
set_dependency("linear_reg", "glm", "stats")

set_fit(
  model = "linear_reg",
  eng = "glm",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(pkg = "stats", fun = "glm"),
    defaults = list(family = expr(stats::gaussian))
  )
)

set_encoding(
  model = "linear_reg",
  eng = "glm",
  mode = "regression",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "linear_reg",
  eng = "glm",
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
  model = "linear_reg",
  eng = "glm",
  mode = "regression",
  type = "conf_int",
  value = list(
    pre = NULL,
    post = linear_lp_to_conf_int,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        se.fit = TRUE,
        type = "link"
      )
  )
)

set_pred(
  model = "linear_reg",
  eng = "glm",
  mode = "regression",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(object = expr(object$fit), newdata = expr(new_data))
  )
)


# ------------------------------------------------------------------------------

set_model_engine("linear_reg", "regression", "glmnet")
set_dependency("linear_reg", "glmnet", "glmnet")

set_fit(
  model = "linear_reg",
  eng = "glmnet",
  mode = "regression",
  value = list(
    interface = "matrix",
    protect = c("x", "y", "weights"),
    func = c(pkg = "glmnet", fun = "glmnet"),
    defaults = list(family = "gaussian")
  )
)

set_encoding(
  model = "linear_reg",
  eng = "glmnet",
  mode = "regression",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = TRUE
  )
)

set_model_arg(
  model = "linear_reg",
  eng = "glmnet",
  parsnip = "penalty",
  original = "lambda",
  func = list(pkg = "dials", fun = "penalty"),
  has_submodel = TRUE
)

set_model_arg(
  model = "linear_reg",
  eng = "glmnet",
  parsnip = "mixture",
  original = "alpha",
  func = list(pkg = "dials", fun = "mixture"),
  has_submodel = FALSE
)

set_pred(
  model = "linear_reg",
  eng = "glmnet",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = .organize_glmnet_pred,
    func = c(fun = "predict"),
    args =
      list(
        object = expr(object$fit),
        newx = expr(organize_glmnet_pre_pred(new_data, object)),
        type = "response",
        s = expr(object$spec$args$penalty)
      )
  )
)

set_pred(
  model = "linear_reg",
  eng = "glmnet",
  mode = "regression",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(object = expr(object$fit),
           newx = expr(as.matrix(new_data)))
  )
)

# ------------------------------------------------------------------------------

set_model_engine("linear_reg", "regression", "stan")
set_dependency("linear_reg", "stan", "rstanarm")

set_fit(
  model = "linear_reg",
  eng = "stan",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(pkg = "rstanarm", fun = "stan_glm"),
    defaults = list(family = expr(stats::gaussian), refresh = 0)
  )
)

set_encoding(
  model = "linear_reg",
  eng = "stan",
  mode = "regression",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "linear_reg",
  eng = "stan",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(object = expr(object$fit), newdata = expr(new_data))
  )
)

set_pred(
  model = "linear_reg",
  eng = "stan",
  mode = "regression",
  type = "conf_int",
  value = list(
    pre = NULL,
    post = function(results, object) {
      res <-
        tibble(
          .pred_lower =
            convert_stan_interval(
              results,
              level = object$spec$method$pred$conf_int$extras$level
            ),
          .pred_upper =
            convert_stan_interval(
              results,
              level = object$spec$method$pred$conf_int$extras$level,
              lower = FALSE
            ),
        )
      if (object$spec$method$pred$conf_int$extras$std_error)
        res$.std_error <- apply(results, 2, sd, na.rm = TRUE)
      res
    },
    func = c(pkg = "parsnip", fun = "stan_conf_int"),
    args =
      list(
        object = expr(object$fit),
        newdata = expr(new_data)
      )
  )
)

set_pred(
  model = "linear_reg",
  eng = "stan",
  mode = "regression",
  type = "pred_int",
  value = list(
    pre = NULL,
    post = function(results, object) {
      res <-
        tibble(
          .pred_lower =
            convert_stan_interval(
              results,
              level = object$spec$method$pred$pred_int$extras$level
            ),
          .pred_upper =
            convert_stan_interval(
              results,
              level = object$spec$method$pred$pred_int$extras$level,
              lower = FALSE
            ),
        )
      if (object$spec$method$pred$pred_int$extras$std_error)
        res$.std_error <- apply(results, 2, sd, na.rm = TRUE)
      res
    },
    func = c(pkg = "rstanarm", fun = "posterior_predict"),
    args =
      list(
        object = expr(object$fit),
        newdata = expr(new_data),
        seed = expr(sample.int(10^5, 1))
      )
  )
)

set_pred(
  model = "linear_reg",
  eng = "stan",
  mode = "regression",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(object = expr(object$fit), newdata = expr(new_data))
  )
)

# ------------------------------------------------------------------------------

set_model_engine("linear_reg", "regression", "spark")
set_dependency("linear_reg", "spark", "sparklyr")

set_fit(
  model = "linear_reg",
  eng = "spark",
  mode = "regression",
  value = list(
    interface = "formula",
    data = c(formula = "formula", data = "x", weights = "weight_col"),
    protect = c("x", "formula", "weights"),
    func = c(pkg = "sparklyr", fun = "ml_linear_regression"),
    defaults = list()
  )
)

set_encoding(
  model = "linear_reg",
  eng = "spark",
  mode = "regression",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

set_model_arg(
  model = "linear_reg",
  eng = "spark",
  parsnip = "penalty",
  original = "reg_param",
  func = list(pkg = "dials", fun = "penalty"),
  has_submodel = FALSE
)

set_model_arg(
  model = "linear_reg",
  eng = "spark",
  parsnip = "mixture",
  original = "elastic_net_param",
  func = list(pkg = "dials", fun = "mixture"),
  has_submodel = FALSE
)

set_pred(
  model = "linear_reg",
  eng = "spark",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = function(results, object) {
      results <- dplyr::rename(results, pred = prediction)
      results <- dplyr::select(results, pred)
      results
    },
    func = c(pkg = "sparklyr", fun = "ml_predict"),
    args = list(x = expr(object$fit), dataset = expr(new_data))
  )
)

# ------------------------------------------------------------------------------


set_model_engine("linear_reg", "regression", "keras")
set_dependency("linear_reg", "keras", "keras")
set_dependency("linear_reg", "keras", "magrittr")

set_fit(
  model = "linear_reg",
  eng = "keras",
  mode = "regression",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    func = c(pkg = "parsnip", fun = "keras_mlp"),
    defaults = list(hidden_units = 1, act = "linear")
  )
)

set_encoding(
  model = "linear_reg",
  eng = "keras",
  mode = "regression",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

set_model_arg(
  model = "linear_reg",
  eng = "keras",
  parsnip = "penalty",
  original = "penalty",
  func = list(pkg = "dials", fun = "penalty"),
  has_submodel = FALSE
)

set_pred(
  model = "linear_reg",
  eng = "keras",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = maybe_multivariate,
    func = c(fun = "predict"),
    args = list(object = quote(object$fit), x = quote(as.matrix(new_data)))
  )
)

# ------------------------------------------------------------------------------


set_model_engine("linear_reg", "regression", "brulee")
set_dependency("linear_reg", "brulee", "brulee")


set_model_arg(
  model = "linear_reg",
  eng = "brulee",
  parsnip = "penalty",
  original = "penalty",
  func = list(pkg = "dials", fun = "penalty"),
  has_submodel = FALSE
)

set_model_arg(
  model = "linear_reg",
  eng = "brulee",
  parsnip = "mixture",
  original = "mixture",
  func = list(pkg = "dials", fun = "mixture"),
  has_submodel = FALSE
)

set_fit(
  model = "linear_reg",
  eng = "brulee",
  mode = "regression",
  value = list(
    interface = "data.frame",
    protect = c("x", "y"),
    func = c(pkg = "brulee", fun = "brulee_linear_reg"),
    defaults = list()
  )
)

set_encoding(
  model = "linear_reg",
  eng = "brulee",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "linear_reg",
  eng = "brulee",
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

# ------------------------------------------------------------------------------

set_model_engine(model = "linear_reg", mode = "quantile regression", eng = "quantreg")
set_dependency(model = "linear_reg", eng = "quantreg", pkg = "quantreg", mode = "quantile regression")

set_fit(
  model = "linear_reg",
  eng = "quantreg",
  mode = "quantile regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(pkg = "quantreg", fun = "rq"),
    defaults = list(tau = expr(quantile_levels))
  )
)

set_encoding(
  model = "linear_reg",
  eng = "quantreg",
  mode = "quantile regression",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "linear_reg",
  eng = "quantreg",
  mode = "quantile regression",
  type = "quantile",
  value = list(
    pre = NULL,
    post = matrix_to_quantile_pred,
    func = c(fun = "predict"),
    args =
      list(
        object = expr(object$fit),
        newdata = expr(new_data)
      )
  )
)
