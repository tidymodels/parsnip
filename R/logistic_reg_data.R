set_new_model("logistic_reg")

set_model_mode("logistic_reg", "classification")

# ------------------------------------------------------------------------------

set_model_engine("logistic_reg", "classification", "glm")
set_dependency("logistic_reg", "glm", "stats")

set_fit(
  model = "logistic_reg",
  eng = "glm",
  mode = "classification",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(pkg = "stats", fun = "glm"),
    defaults = list(family = expr(stats::binomial))
  )
)

set_pred(
  model = "logistic_reg",
  eng = "glm",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = prob_to_class_2,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "response"
      )
  )
)

set_pred(
  model = "logistic_reg",
  eng = "glm",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = function(x, object) {
      x <- tibble(v1 = 1 - x, v2 = x)
      colnames(x) <- object$lvl
      x
    },
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "response"
      )
  )
)

set_pred(
  model = "logistic_reg",
  eng = "glm",
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

set_pred(
  model = "logistic_reg",
  eng = "glm",
  mode = "classification",
  type = "conf_int",
  value = list(
    pre = NULL,
    post = function(results, object) {
      hf_lvl <- (1 - object$spec$method$pred$conf_int$extras$level)/2
      const <-
        qt(hf_lvl, df = object$fit$df.residual, lower.tail = FALSE)
      trans <- object$fit$family$linkinv
      res_2 <-
        tibble(
          lo = trans(results$fit - const * results$se.fit),
          hi = trans(results$fit + const * results$se.fit)
        )
      res_1 <- res_2
      res_1$lo <- 1 - res_2$hi
      res_1$hi <- 1 - res_2$lo
      res <- bind_cols(res_1, res_2)
      lo_nms <- paste0(".pred_lower_", object$lvl)
      hi_nms <- paste0(".pred_upper_", object$lvl)
      colnames(res) <- c(lo_nms[1], hi_nms[1], lo_nms[2], hi_nms[2])

      if (object$spec$method$pred$conf_int$extras$std_error)
        res$.std_error <- results$se.fit
      res
    },
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

# ------------------------------------------------------------------------------

set_model_engine("logistic_reg", "classification", "glmnet")
set_dependency("logistic_reg", "glmnet", "glmnet")

set_model_arg(
  model = "logistic_reg",
  eng = "glmnet",
  parsnip = "penalty",
  original = "lambda",
  func = list(pkg = "dials", fun = "penalty"),
  has_submodel = TRUE
)

set_model_arg(
  model = "logistic_reg",
  eng = "glmnet",
  parsnip = "mixture",
  original = "alpha",
  func = list(pkg = "dials", fun = "mixture"),
  has_submodel = FALSE
)

set_fit(
  model = "logistic_reg",
  eng = "glmnet",
  mode = "classification",
  value = list(
    interface = "matrix",
    protect = c("x", "y", "weights"),
    func = c(pkg = "glmnet", fun = "glmnet"),
    defaults = list(family = "binomial")
  )
)


set_pred(
  model = "logistic_reg",
  eng = "glmnet",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = organize_glmnet_class,
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
  model = "logistic_reg",
  eng = "glmnet",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = organize_glmnet_prob,
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
  model = "logistic_reg",
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

set_model_engine("logistic_reg", "classification", "spark")
set_dependency("logistic_reg", "spark", "sparklyr")

set_model_arg(
  model = "logistic_reg",
  eng = "spark",
  parsnip = "penalty",
  original = "reg_param",
  func = list(pkg = "dials", fun = "penalty"),
  has_submodel = TRUE
)

set_model_arg(
  model = "logistic_reg",
  eng = "spark",
  parsnip = "mixture",
  original = "elastic_net_param",
  func = list(pkg = "dials", fun = "mixture"),
  has_submodel = FALSE
)

set_fit(
  model = "logistic_reg",
  eng = "spark",
  mode = "classification",
  value = list(
    interface = "formula",
    protect = c("x", "formula", "weight_col"),
    func = c(pkg = "sparklyr", fun = "ml_logistic_regression"),
    defaults =
      list(
        family = "binomial"
      )
  )
)

set_pred(
  model = "logistic_reg",
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
  model = "logistic_reg",
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

set_model_engine("logistic_reg", "classification", "keras")
set_dependency("logistic_reg", "keras", "keras")
set_dependency("logistic_reg", "keras", "magrittr")

set_model_arg(
  model = "logistic_reg",
  eng = "keras",
  parsnip = "decay",
  original = "decay",
  func = list(pkg = "dials", fun = "weight_decay"),
  has_submodel = FALSE
)

set_fit(
  model = "logistic_reg",
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
  model = "logistic_reg",
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
  model = "logistic_reg",
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

# ------------------------------------------------------------------------------

set_model_engine("logistic_reg", "classification", "stan")
set_dependency("logistic_reg", "stan", "rstanarm")

set_fit(
  model = "logistic_reg",
  eng = "stan",
  mode = "classification",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(pkg = "rstanarm", fun = "stan_glm"),
    defaults = list(family = expr(stats::binomial))
  )
)

set_pred(
  model = "logistic_reg",
  eng = "stan",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = function(x, object) {
      x <- object$fit$family$linkinv(x)
      x <- ifelse(x >= 0.5, object$lvl[2], object$lvl[1])
      unname(x)
    },
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
  )
)

set_pred(
  model = "logistic_reg",
  eng = "stan",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = function(x, object) {
      x <- object$fit$family$linkinv(x)
      x <- tibble(v1 = 1 - x, v2 = x)
      colnames(x) <- object$lvl
      x
    },
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
  )
)


set_pred(
  model = "logistic_reg",
  eng = "stan",
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

set_pred(
  model = "logistic_reg",
  eng = "stan",
  mode = "classification",
  type = "conf_int",
  value = list(
    pre = NULL,
    post = function(results, object) {
      res_2 <-
        tibble(
          lo =
            convert_stan_interval(
              results,
              level = object$spec$method$pred$conf_int$extras$level
            ),
          hi =
            convert_stan_interval(
              results,
              level = object$spec$method$pred$conf_int$extras$level,
              lower = FALSE
            ),
        )
      res_1 <- res_2
      res_1$lo <- 1 - res_2$hi
      res_1$hi <- 1 - res_2$lo
      res <- bind_cols(res_1, res_2)
      lo_nms <- paste0(".pred_lower_", object$lvl)
      hi_nms <- paste0(".pred_upper_", object$lvl)
      colnames(res) <- c(lo_nms[1], hi_nms[1], lo_nms[2], hi_nms[2])

      if (object$spec$method$pred$conf_int$extras$std_error)
        res$.std_error <- apply(results, 2, sd, na.rm = TRUE)
      res
    },
    func = c(pkg = "rstanarm", fun = "posterior_linpred"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        transform = TRUE,
        seed = expr(sample.int(10^5, 1))
      )
  )
)

set_pred(
  model = "logistic_reg",
  eng = "stan",
  mode = "classification",
  type = "pred_int",
  value = list(
    pre = NULL,
    post = function(results, object) {
      res_2 <-
        tibble(
          lo =
            convert_stan_interval(
              results,
              level = object$spec$method$pred$pred_int$extras$level
            ),
          hi =
            convert_stan_interval(
              results,
              level = object$spec$method$pred$pred_int$extras$level,
              lower = FALSE
            ),
        )
      res_1 <- res_2
      res_1$lo <- 1 - res_2$hi
      res_1$hi <- 1 - res_2$lo
      res <- bind_cols(res_1, res_2)
      lo_nms <- paste0(".pred_lower_", object$lvl)
      hi_nms <- paste0(".pred_upper_", object$lvl)
      colnames(res) <- c(lo_nms[1], hi_nms[1], lo_nms[2], hi_nms[2])

      if (object$spec$method$pred$pred_int$extras$std_error)
        res$.std_error <- apply(results, 2, sd, na.rm = TRUE)
      res
    },
    func = c(pkg = "rstanarm", fun = "posterior_predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        seed = expr(sample.int(10^5, 1))
      )
  )
)
