
multinom_reg_arg_key <- data.frame(
  glmnet =  c(   "lambda",             "alpha"),
  spark  =  c("reg_param", "elastic_net_param"),
  keras  =  c(    "decay",                  NA),  
  stringsAsFactors = FALSE,
  row.names =  c("penalty", "mixture")
)

multinom_reg_modes <- "classification"

multinom_reg_engines <- data.frame(
  glmnet = TRUE,
  spark  = TRUE,
  keras  = TRUE,
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
    class = list(
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
    classprob = list(
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


multinom_reg_keras_data <-
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
