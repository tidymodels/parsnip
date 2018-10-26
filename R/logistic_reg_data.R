
logistic_reg_arg_key <- data.frame(
  glm    =  c(        NA,                  NA),
  glmnet =  c(   "lambda",             "alpha"),
  spark  =  c("reg_param", "elastic_net_param"),
  stan   =  c(        NA,                  NA),
  keras  =  c(   "decay",                  NA),  
  stringsAsFactors = FALSE,
  row.names =  c("penalty", "mixture")
)

logistic_reg_modes <- "classification"

logistic_reg_engines <- data.frame(
  glm    = TRUE,
  glmnet = TRUE,
  spark  = TRUE,
  stan   = TRUE,
  keras  = TRUE,
  row.names =  c("classification")
)

# ------------------------------------------------------------------------------

#' @importFrom stats qt
logistic_reg_glm_data <-
  list(
    libs = "stats",
    fit = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "stats", fun = "glm"),
      defaults =
        list(
          family = expr(stats::binomial)
        )
    ),
    class = list(
      pre = NULL,
      post = prob_to_class_2,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "response"
        )
    ),
    classprob = list(
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
    ),
    raw = list(
      pre = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    ),
    confint = list(
      pre = NULL,
      post = function(results, object) {
        hf_lvl <- (1 - object$spec$method$confint$extras$level)/2
        const <-
          qt(hf_lvl, df = object$fit$df.residual, lower.tail = FALSE)
        trans <- object$fit$family$linkinv
        res <-
          tibble(
            .pred_lower = trans(results$fit - const * results$se.fit),
            .pred_upper = trans(results$fit + const * results$se.fit)
          )
        if(object$spec$method$confint$extras$std_error)
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

# Note: For glmnet, you will need to make model-specific predict methods.
# See logistic_reg.R
logistic_reg_glmnet_data <-
  list(
    libs = "glmnet",
    fit = list(
      interface = "matrix",
      protect = c("x", "y", "weights"),
      func = c(pkg = "glmnet", fun = "glmnet"),
      defaults =
        list(
          family = "binomial"
        )
    ),
    class = list(
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
    ),
    classprob = list(
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
    ),
    raw = list(
      pre = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newx = quote(as.matrix(new_data))
        )
    )
  )

logistic_reg_stan_data <-
  list(
    libs = "rstanarm",
    fit = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "rstanarm", fun = "stan_glm"),
      defaults =
        list(
          family = expr(stats::binomial)
        )
    ),
    class = list(
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
    ),
    classprob = list(
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
    ),
    raw = list(
      pre = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    ),
    confint = list(
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
          object = quote(object$fit),
          newdata = quote(new_data),
          transform = TRUE,
          seed = expr(sample.int(10^5, 1))
        )
    ),
    predint = list(
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
          object = quote(object$fit),
          newdata = quote(new_data),
          seed = expr(sample.int(10^5, 1))
        )
    )
  )


logistic_reg_spark_data <-
  list(
    libs = "sparklyr",
    fit = list(
      interface = "formula",
      protect = c("x", "formula", "weight_col"),
      func = c(pkg = "sparklyr", fun = "ml_logistic_regression"),
      defaults =
        list(
          family = "binomial"
        )
    ),
    class = list(
      pre = NULL,
      post = format_spark_class,
      func = c(pkg = "sparklyr", fun = "ml_predict"),
      args =
        list(
          x = quote(object$fit),
          dataset = quote(new_data)
        )
    ),
    classprob = list(
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

logistic_reg_keras_data <-
  list(
    libs = c("keras", "magrittr"),
    fit = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(pkg = "parsnip", fun = "keras_mlp"),
      defaults = list(hidden_units = 1, act = "linear")
    ),
    class = list(
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
    ),
    classprob = list(
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