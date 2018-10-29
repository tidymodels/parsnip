
decision_tree_arg_key <- data.frame(
  rpart =   c( "maxdepth",               "minsplit",              "cp"),
  C5.0 =    c(         NA,               "minCases",                NA),
  spark =   c("max_depth", "min_instances_per_node",                NA),
  stringsAsFactors = FALSE,
  row.names =  c("tree_depth",              "min_n", "cost_complexity")
)

decision_tree_modes <- c("classification", "regression", "unknown")

decision_tree_engines <- data.frame(
  rpart   =    rep(TRUE, 3),
  C5.0    =    c(TRUE, FALSE, TRUE),
  spark   =    rep(TRUE, 3),
  row.names =  c("classification", "regression", "unknown")
)

# ------------------------------------------------------------------------------

decision_tree_rpart_data <-
  list(
    libs = "rpart",
    fit = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "rpart", fun = "rpart"),
      defaults =
        list()
    ),
    numeric = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    ),
    class = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = NULL, fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "class"
        )
    ),
    classprob = list(
      pre = NULL,
      post = function(x, object) {
        as_tibble(x)
      },
      func = c(pkg = NULL, fun = "predict"),
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
    )
  )


decision_tree_C5.0_data <-
  list(
    libs = "C50",
    fit = list(
      interface = "data.frame",
      protect = c("x", "y", "weights"),
      func = c(pkg = "parsnip", fun = "C5.0_train"),
      defaults = list(trials = 1)
    ),
    class = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    ),
    classprob = list(
      pre = NULL,
      post = function(x, object) {
        as_tibble(x)
      },
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "prob"
        )
    ),
    raw = list(
      pre = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )


decision_tree_spark_data <-
  list(
    libs = "sparklyr",
    fit = list(
      interface = "formula",
      protect = c("x", "formula"),
      func = c(pkg = "sparklyr", fun = "ml_decision_tree_classifier"),
      defaults =
        list(
          seed = expr(sample.int(10^5, 1))
        )
    ),
    numeric = list(
      pre = NULL,
      post = format_spark_num,
      func = c(pkg = "sparklyr", fun = "ml_predict"),
      args =
        list(
          x = quote(object$fit),
          dataset = quote(new_data)
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
