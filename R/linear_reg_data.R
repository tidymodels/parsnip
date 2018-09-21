
linear_reg_arg_key <- data.frame(
  lm     =  c(        NA,                  NA),
  glmnet =  c(   "lambda",             "alpha"),
  spark  =  c("reg_param", "elastic_net_param"),
  stan   =  c(        NA,                  NA),
  stringsAsFactors = FALSE,
  row.names =  c("penalty", "mixture")
)

linear_reg_modes <- "regression"

linear_reg_engines <- data.frame(
  lm    = TRUE,
  glmnet = TRUE,
  spark  = TRUE,
  stan   = TRUE,
  row.names =  c("regression")
)

# ------------------------------------------------------------------------------

organize_glmnet_pred <- function(x, object) {
  if (ncol(x) == 1) {
    res <- x[, 1]
    res <- unname(res)
  } else {
    n <- nrow(x)
    res <- utils::stack(as.data.frame(x))
    if (!is.null(object$spec$args$penalty))
      res$lambda <- rep(object$spec$args$penalty, each = n) else
        res$lambda <- rep(object$fit$lambda, each = n)
    res <- res[, colnames(res) %in% c("values", "lambda")]
  }
  res
}

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
    pred = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
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
          object = quote(object$fit),
          newdata = quote(new_data),
          interval = "confidence",
          level = quote(level),
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
          object = quote(object$fit),
          newdata = quote(new_data),
          interval = "prediction",
          level = quote(level),
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
    )
  )

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
    pred = list(
      pre = NULL,
      post = organize_glmnet_pred,
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

linear_reg_stan_data <-
  list(
    libs = "rstanarm",
    fit = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "rstanarm", fun = "stan_glm"),
      defaults =
        list(
          family = "gaussian"
        )
    ),
    pred = list(
      pre = NULL,
      post = NULL,
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
    ),
    raw = list(
      pre = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )


linear_reg_spark_data <-
  list(
    libs = "sparklyr",
    fit = list(
      interface = "formula",
      protect = c("x", "formula", "weight_col"),
      func = c(pkg = "sparklyr", fun = "ml_linear_regression"),
      defaults = list()
    )
  )



