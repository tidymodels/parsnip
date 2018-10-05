nearest_neighbor_arg_key <- data.frame(
  kknn      =  c("ks",        "kernel",      "distance"),
  row.names =  c("neighbors", "weight_func", "dist_power"),
  stringsAsFactors = FALSE
)

nearest_neighbor_modes <- c("classification", "regression", "unknown")

nearest_neighbor_engines <- data.frame(
  kknn =       c(TRUE, TRUE, FALSE),
  row.names =  c("classification", "regression", "unknown")
)

# ------------------------------------------------------------------------------

nearest_neighbor_kknn_data <-
  list(
    libs = "kknn",
    fit = list(
      interface = "formula",
      protect = c("formula", "data", "kmax"), # kmax is not allowed
      func = c(pkg = "kknn", fun = "train.kknn"),
      defaults = list()
    )
  )
