
boost_tree_arg_key <- data.frame(
  xgboost = c("max_depth",  "nrounds",       "eta",       "colsample_bytree",        "min_child_weight",         "gamma",        "subsample"),
  C5.0 =    c(         NA,   "trials",          NA,                       NA,                "minCases",              NA,           "sample"),
  spark =   c("max_depth", "max_iter", "step_size", "feature_subset_strategy", "min_instances_per_node", "min_info_gain", "subsampling_rate"),
  stringsAsFactors = FALSE,
  row.names =  c("tree_depth", "trees", "learn_rate", "mtry", "min_n", "loss_reduction", "sample_size")
)

boost_tree_modes <- c("classification", "regression", "unknown")

boost_tree_engines <- data.frame(
  xgboost =    rep(TRUE, 3),
  C5.0    =    c(            TRUE,        FALSE,      TRUE),
  spark   =    rep(TRUE, 3),
  row.names =  c("classification", "regression", "unknown")
)

# ------------------------------------------------------------------------------

boost_tree_xgboost_data <-
  list(
    libs = "xgboost",
    fit = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(pkg = NULL, fun = "xgb_train"),
      defaults =
        list(
          nthread = 1,
          verbose = 0
        )
    ),
    pred = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "xgb_pred"),
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data)
        )
    ),
    classes = list(
      pre = NULL,
      post = function(x, object) {
        if (is.vector(x)) {
          x <- ifelse(x >= 0.5, object$lvl[2], object$lvl[1])
        } else {
          x <- object$lvl[apply(x, 1, which.max)]
        }
        x
      },
      func = c(pkg = NULL, fun = "xgb_pred"),
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data)
        )
    ),
    prob = list(
      pre = NULL,
      post = function(x, object) {
        if (is.vector(x)) {
          x <- tibble(v1 = 1 - x, v2 = x)
        } else {
          x <- as_tibble(x)
        }
        colnames(x) <- object$lvl
        x
      },
      func = c(pkg = NULL, fun = "xgb_pred"),
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data)
        )
    ),
    raw = list(
      pre = NULL,
      func = c(fun = "xgb_pred"),
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data)
        )
    )
  )


boost_tree_C5.0_data <-
  list(
    libs = "C50",
    fit = list(
      interface = "data.frame",
      protect = c("x", "y", "weights"),
      func = c(pkg = NULL, fun = "C5.0_train"),
      defaults = list()
    ),
    classes = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = expr(object$fit),
        newdata = expr(new_data)
      )
    ),
    prob = list(
      pre = NULL,
      post = function(x, object) {
        as_tibble(x)
      },
      func = c(fun = "predict"),
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data),
          type = "prob"
        )
    ),
    raw = list(
      pre = NULL,
      func = c(fun = "predict"),
      args = list(
        object = expr(object$fit),
        newdata = expr(new_data)
      )
    )
  )


boost_tree_spark_data <-
  list(
    libs = "sparklyr",
    fit = list(
      interface = "formula",
      protect = c("x", "formula", "type"),
      func = c(pkg = "sparklyr", fun = "ml_gradient_boosted_trees"),
      defaults =
        list(
          seed = expr(sample.int(10^5, 1))
        )
    ),
    pred = list(
      pre = NULL,
      post = format_spark_num,
      func = c(pkg = "sparklyr", fun = "ml_predict"),
      args =
        list(
          x = expr(object$fit),
          dataset = expr(new_data)
        )
    ),
    classes = list(
      pre = NULL,
      post = format_spark_class,
      func = c(pkg = "sparklyr", fun = "ml_predict"),
      args =
        list(
          x = expr(object$fit),
          dataset = expr(new_data)
        )
    ),
    prob = list(
      pre = NULL,
      post = format_spark_probs,
      func = c(pkg = "sparklyr", fun = "ml_predict"),
      args =
        list(
          x = expr(object$fit),
          dataset = expr(new_data)
        )
    )
  )
