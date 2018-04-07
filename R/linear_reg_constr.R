
linear_reg_arg_key <- data.frame(
  lm     =  c(        NA,                  NA),
  glmnet =  c(   "lambda",             "alpha"),
  spark  =  c("reg_param", "elastic_net_param"),
  stan   =  c(        NA,                  NA),
  stringsAsFactors = FALSE,
  row.names =  c("regularization", "mixture")
)

linear_reg_modes <- "regression"

linear_reg_engines <- data.frame(
  lm    = TRUE,
  glmnet = TRUE,
  spark  = TRUE,
  stan   = TRUE,
  row.names =  c("regression")
)

###################################################################

linear_reg_lm_fit <-
  list(
    libs = "stats",
    interface = "formula",
    protect = c("formula", "data", "weights"),
    fit_name = c(pkg = "stats", fun = "lm"),
    alternates = list()
  )

linear_reg_glmnet_fit <-
  list(
    libs = "glmnet",
    interface = "matrix",
    protect = c("x", "y", "weights"),
    fit_name = c(pkg = "glmnet", fun = "glmnet"),
    alternates =
      list(
        family = "gaussian"
      )
  )

linear_reg_stan_fit <-
  list(
    libs = "rstanarm",
    interface = "formula",
    protect = c("formula", "data", "weights"),
    fit_name = c(pkg = "rstanarm", fun = "stan_glm"),
    alternates =
      list(
        family = "gaussian"
      )
  )


linear_reg_spark_fit <-
  list(
    libs = "sparklyr",
    interface = "formula",
    protect = c("x", "formula", "weight_col"),
    fit_name = c(pkg = "sparklyr", fun = "ml_linear_regression"),
    alternates = list()
  )
