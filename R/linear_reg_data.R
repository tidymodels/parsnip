set_new_model("linear_reg")

set_model_mode("linear_reg", "regression")

# ------------------------------------------------------------------------------

set_model_engine("linear_reg", "regression", "lm")
set_dependency("linear_reg", "lm", "stats")

set_fit(
  mod = "linear_reg",
  eng = "lm",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(pkg = "stats", fun = "lm"),
    defaults = list()
  )
)

set_pred(
  mod = "linear_reg",
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
        type = "response"
      )
  )
)

set_pred(
  mod = "linear_reg",
  eng = "lm",
  mode = "regression",
  type = "conf_int",
  value = list(
    pre = NULL,
    post = function(results, object) {
      tibble::as_tibble(results) %>%
        dplyr::select(-fit) %>%
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
  mod = "linear_reg",
  eng = "lm",
  mode = "regression",
  type = "pred_int",
  value = list(
    pre = NULL,
    post = function(results, object) {
      tibble::as_tibble(results) %>%
        dplyr::select(-fit) %>%
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
  mod = "linear_reg",
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

set_model_engine("linear_reg", "regression", "glmnet")
set_dependency("linear_reg", "glmnet", "glmnet")

set_model_arg(
  mod = "linear_reg",
  eng = "glmnet",
  val = "penalty",
  original = "lambda",
  func = list(pkg = "dials", fun = "penalty"),
  submodels = TRUE
)

set_model_arg(
  mod = "linear_reg",
  eng = "glmnet",
  val = "mixture",
  original = "alpha",
  func = list(pkg = "dials", fun = "mixture"),
  submodels = FALSE
)

set_fit(
  mod = "linear_reg",
  eng = "glmnet",
  mode = "regression",
  value = list(
    interface = "matrix",
    protect = c("x", "y", "weights"),
    func = c(pkg = "glmnet", fun = "glmnet"),
    defaults = list(family = "gaussian")
  )
)

set_pred(
  mod = "linear_reg",
  eng = "glmnet",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = organize_glmnet_pred,
    func = c(fun = "predict"),
    args =
      list(
        object = expr(object$fit),
        newx = expr(as.matrix(new_data)),
        type = "response",
        s = expr(object$spec$args$penalty)
      )
  )
)

set_pred(
  mod = "linear_reg",
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
  mod = "linear_reg",
  eng = "stan",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(pkg = "rstanarm", fun = "stan_glm"),
    defaults = list(family = expr(stats::gaussian))
  )
)

set_pred(
  mod = "linear_reg",
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
  mod = "linear_reg",
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
              level = object$spec$method$confint$extras$level
            ),
          .pred_upper =
            convert_stan_interval(
              results,
              level = object$spec$method$confint$extras$level,
              lower = FALSE
            ),
        )
      if(object$spec$method$confint$extras$std_error)
        res$.std_error <- apply(results, 2, sd, na.rm = TRUE)
      res
    },
    func = c(pkg = "rstanarm", fun = "posterior_linpred"),
    args =
      list(
        object = expr(object$fit),
        newdata = expr(new_data),
        transform = TRUE,
        seed = expr(sample.int(10^5, 1))
      )
  )
)

set_pred(
  mod = "linear_reg",
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
              level = object$spec$method$predint$extras$level
            ),
          .pred_upper =
            convert_stan_interval(
              results,
              level = object$spec$method$predint$extras$level,
              lower = FALSE
            ),
        )
      if(object$spec$method$predint$extras$std_error)
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
  mod = "linear_reg",
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
  mod = "linear_reg",
  eng = "spark",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("x", "formula", "weight_col"),
    func = c(pkg = "sparklyr", fun = "ml_linear_regression")
  )
)

set_pred(
  mod = "linear_reg",
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
set_dependency("linear_reg", "keras", c("keras", "magrittr"))

set_fit(
  mod = "linear_reg",
  eng = "keras",
  mode = "regression",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    func = c(pkg = "parsnip", fun = "keras_mlp"),
    defaults = list(hidden_units = 1, act = "linear")
  )
)

set_pred(
  mod = "linear_reg",
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

