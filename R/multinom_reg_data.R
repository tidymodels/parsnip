
multinom_reg_arg_key <- data.frame(
  glmnet =  c(   "lambda",             "alpha"),
  spark  =  c("reg_param", "elastic_net_param"),
  stringsAsFactors = FALSE,
  row.names =  c("penalty", "mixture")
)

multinom_reg_modes <- "classification"

multinom_reg_engines <- data.frame(
  glmnet = TRUE,
  spark  = TRUE,
  row.names =  c("classification")
)

# ------------------------------------------------------------------------------

multinom_reg_glmnet_data <-
  list(
    libs = "glmnet",
    fit = list(
      interface = "matrix",
      protect = c("x", "y", "weights"),
      func = c(pkg = "glmnet", fun = "glmnet"),
      defaults =
        list(
          family = "multinomial"
        )
    ),
    classes = list(
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
    ),
    prob = list(
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

multinom_reg_spark_data <-
  list(
    libs = "sparklyr",
    fit = list(
      interface = "formula",
      protect = c("x", "formula", "weight_col"),
      func = c(pkg = "sparklyr", fun = "ml_logistic_regression"),
      defaults =
        list(
          family = "multinomial"
        )
    ),
    classes = list(
      pre = NULL,
      post = format_spark_class,
      func = c(pkg = "sparklyr", fun = "ml_predict"),
      args =
        list(
          x = quote(object$fit),
          dataset = quote(new_data)
        )
    ),
    prob = list(
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


