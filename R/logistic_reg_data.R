
logistic_reg_arg_key <- data.frame(
  glm    =  c(        NA,                  NA),
  glmnet =  c(   "lambda",             "alpha"),
  spark  =  c("reg_param", "elastic_net_param"),
  stan   =  c(        NA,                  NA),
  stringsAsFactors = FALSE,
  row.names =  c("regularization", "mixture")
)

logistic_reg_modes <- "classification"

logistic_reg_engines <- data.frame(
  glm    = TRUE,
  glmnet = TRUE,
  spark  = TRUE,
  stan   = TRUE,
  row.names =  c("classification")
)

###################################################################

prob_to_class_2 <- function(x, object) {
  x <- ifelse(x >= 0.5, object$lvl[2], object$lvl[1])
  unname(x)
}

organize_glmnet_class <- function(x, object) {
  if (ncol(x) == 1) {
    res <- prob_to_class_2(x[, 1], object)
  } else {
    n <- nrow(x)
    res <- utils::stack(as.data.frame(x))
    res$values <- prob_to_class_2(res$values, object)
    if (!is.null(object$spec$args$regularization))
      res$lambda <- rep(object$spec$args$regularization, each = n) else
        res$lambda <- rep(object$fit$lambda, each = n)
    res <- res[, colnames(res) %in% c("values", "lambda")]
  }
  res
}

organize_glmnet_prob <- function(x, object) {
  if (ncol(x) == 1) {
    res <- tibble(v1 = 1 - x[, 1], v2 = x[, 1])
    colnames(res) <- object$lvl
  } else {
    n <- nrow(x)
    res <- utils::stack(as.data.frame(x))
    res <- tibble(v1 = 1 - res$values, v2 = res$values)
    colnames(res) <- object$lvl
    if (!is.null(object$spec$args$regularization))
      res$lambda <- rep(object$spec$args$regularization, each = n) else
        res$lambda <- rep(object$fit$lambda, each = n)
  }
  res
}

###################################################################

logistic_reg_glm_data <-
  list(
    libs = "stats",
    fit = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "stats", fun = "glm"),
      defaults =
        list(
          family = expr(binomial)
        )
    ),
    classes = list(
      pre = NULL,
      post = prob_to_class_2,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(newdata),
          type = "response"
        )
    ),
    prob = list(
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
          newdata = quote(newdata),
          type = "response"
        )
    )
  )

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
    classes = list(
      pre = NULL,
      post = organize_glmnet_class,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newx = quote(as.matrix(newdata)),
          type = "response",
          s = quote(object$spec$args$regularization)
        )
    ),
    prob = list(
      pre = NULL,
      post = organize_glmnet_prob,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newx = quote(as.matrix(newdata)),
          type = "response",
          s = quote(object$spec$args$regularization)
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
          family = expr(binomial)
        )
    ),
    classes = list(
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
          newdata = quote(newdata)
        )
    ),
    prob = list(
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
          newdata = quote(newdata)
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
    )
  )


