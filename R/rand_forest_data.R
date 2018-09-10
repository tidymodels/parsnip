
rand_forest_arg_key <- data.frame(
  randomForest = c("mtry", "ntree", "nodesize"),
  ranger = c("mtry", "num.trees", "min.node.size"),
  spark =
    c("feature_subset_strategy", "num_trees", "min_instances_per_node"),
  stringsAsFactors = FALSE,
  row.names =  c("mtry", "trees", "min_n")
)

rand_forest_modes <- c("classification", "regression", "unknown")

rand_forest_engines <- data.frame(
  ranger =       c(TRUE, TRUE, FALSE),
  randomForest = c(TRUE, TRUE, FALSE),
  spark =        c(TRUE, TRUE, FALSE),
  row.names =  c("classification", "regression", "unknown")
)

###################################################################

ranger_class_pred <-
  function(results, object)  {
    if (results$treetype == "Probability estimation") {
      res <- colnames(results$predictions)[apply(results$predictions, 1, which.max)]
    } else {
      res <- results$predictions
    }
    res
  }

rand_forest_ranger_data <-
  list(
    libs = "ranger",
    fit = list(
      interface = "formula",
      protect = c("formula", "data", "case.weights"),
      func = c(pkg = "ranger", fun = "ranger"),
      defaults =
        list(
          num.threads = 1,
          verbose = FALSE,
          seed = expr(sample.int(10^5, 1))
        )
    ),
    pred = list(
      pre = NULL,
      post = function(results, object) results$predictions,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          data = quote(new_data),
          type = "response",
          seed = expr(sample.int(10^5, 1)),
          verbose = FALSE
        )
    ),
    classes = list(
      pre = NULL,
      post = ranger_class_pred,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          data = quote(new_data),
          type = "response",
          seed = expr(sample.int(10^5, 1)),
          verbose = FALSE
        )
    ),
    prob = list(
      pre = function(x, object) {
        if (object$fit$forest$treetype != "Probability estimation")
          stop("`ranger` model does not appear to use class probabilities. Was ",
               "the model fit with `probability = TRUE`?",
               call. = FALSE)
        x
      },
      post = function(x, object) {
        x <- x$prediction
        as_tibble(x)
      },
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          data = quote(new_data),
          seed = expr(sample.int(10^5, 1)),
          verbose = FALSE
        )
    ),
    raw = list(
      pre = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          data = quote(new_data)
        )
    )
  )

rand_forest_randomForest_data <-
  list(
    libs = "randomForest",
    fit = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "randomForest", fun = "randomForest"),
      defaults =
        list()
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
    classes = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
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
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "prob"
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


rand_forest_spark_data <-
  list(
    libs = "sparklyr",
    fit = list(
      interface = "formula",
      protect = c("x", "formula", "type"),
      func = c(pkg = "sparklyr", fun = "ml_random_forest"),
      defaults =
        list(
          seed = expr(sample.int(10^5, 1))
        )
    )
  )
