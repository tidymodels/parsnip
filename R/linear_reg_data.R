
linear_reg_arg_key <- data.frame(
  lm     =  c(        NA,                  NA),
  glmnet =  c(   "lambda",             "alpha"),
  spark  =  c("reg_param", "elastic_net_param"),
  stan   =  c(        NA,                  NA),
  keras  =  c(   "decay",                  NA),
  stringsAsFactors = FALSE,
  row.names =  c("penalty", "mixture")
)

linear_reg_modes <- "regression"

linear_reg_engines <- data.frame(
  lm     = TRUE,
  glmnet = TRUE,
  spark  = TRUE,
  stan   = TRUE,
  keras  = TRUE,
  row.names =  c("regression")
)


# ------------------------------------------------------------------------------

linear_reg_lm_data <-
  list(
    libs = "stats",
    fit = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "stats", fun = "lm"),
      defaults = list()
    ),
    numeric = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data),
          type = "response"
        )
    ),
    confint = list(
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
    ),
    predint = list(
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
    ),
    raw = list(
      pre = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data)
        )
    )
  )

# Note: For glmnet, you will need to make model-specific predict methods.
# See linear_reg.R
linear_reg_glmnet_data <-
  list(
    libs = "glmnet",
    fit = list(
      interface = "matrix",
      protect = c("x", "y", "weights"),
      func = c(pkg = "glmnet", fun = "glmnet"),
      defaults =
        list(
          family = "gaussian"
        )
    ),
    numeric = list(
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
    ),
    raw = list(
      pre = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = expr(object$fit),
          newx = expr(as.matrix(new_data))
        )
    )
  )

linear_reg_stan_data <-
  list(
    libs = "rstanarm",
    fit = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "rstanarm", fun = "stan_glm"),
      defaults =
        list(
          family = expr(stats::gaussian)
        )
    ),
    numeric = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data)
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
          object = expr(object$fit),
          newdata = expr(new_data),
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
          object = expr(object$fit),
          newdata = expr(new_data),
          seed = expr(sample.int(10^5, 1))
        )
    ),
    raw = list(
      pre = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data)
        )
    )
  )

#' @importFrom  dplyr select rename
linear_reg_spark_data <-
  list(
    libs = "sparklyr",
    fit = list(
      interface = "formula",
      protect = c("x", "formula", "weight_col"),
      func = c(pkg = "sparklyr", fun = "ml_linear_regression")
    ),
    numeric = list(
      pre = NULL,
      post = function(results, object) {
        results <- dplyr::rename(results, pred = prediction)
        results <- dplyr::select(results, pred)
        results
      },
      func = c(pkg = "sparklyr", fun = "ml_predict"),
      args =
        list(
          x = expr(object$fit),
          dataset = expr(new_data)
        )
    )
  )

linear_reg_keras_data <-
  list(
    libs = c("keras", "magrittr"),
    fit = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(pkg = "parsnip", fun = "keras_mlp"),
      defaults = list(hidden_units = 1, act = "linear")
    ),
    numeric = list(
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

