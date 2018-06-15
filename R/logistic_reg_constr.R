
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

logistic_reg_glm_fit <-
  list(
    libs = "stats",
    fit = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "stats", fun = "glm"),
      alternates =
        list(
          family = expr(binomial)
        )
    ), 
    pred = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          data = quote(newdata),
          type = "response"
        )
    )
  )

logistic_reg_glmnet_fit <-
  list(
    libs = "glmnet",
    fit = list(
      interface = "matrix",
      protect = c("x", "y", "weights"),
      func = c(pkg = "glmnet", fun = "glmnet"),
      alternates =
        list(
          family = "binomial"
        )
    )
  )

logistic_reg_stan_fit <-
  list(
    libs = "rstanarm",
    fit = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "rstanarm", fun = "stan_glm"),
      alternates =
        list(
          family = expr(binomial)
        )
    )
  )


logistic_reg_spark_fit <-
  list(
    libs = "sparklyr",
    fit = list(
      interface = "formula",
      protect = c("x", "formula", "weight_col"),
      func = c(pkg = "sparklyr", fun = "ml_logistic_regression"),
      alternates =
        list(
          family = "binomial"
        )
    )
  )
