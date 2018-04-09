
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


rand_forest_ranger_fit <-
  list(
    libs = "ranger",
    interface = "formula",
    protect = c("formula", "data", "case.weights"),
    fit_name = c(pkg = "ranger", fun = "ranger"),
    alternates =
      list(
        num.threads = 1,
        verbose = FALSE,
        seed = expr(sample.int(10^5, 1))
      )
  )

rand_forest_randomForest_fit <-
  list(
    libs = "randomForest",
    interface = "data.frame",
    protect = c("x", "y"),
    fit_name = c(pkg = "randomForest", fun = "randomForest"),
    alternates =
      list()
  )


rand_forest_spark_fit <-
  list(
    libs = "sparklyr",
    interface = "formula",
    protect = c("x", "formula", "type"),
    fit_name = c(pkg = "sparklyr", fun = "ml_random_forest"),
    alternates =
      list(
        seed = expr(sample.int(10^5, 1))
      )
  )
