
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

# ------------------------------------------------------------------------------

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
