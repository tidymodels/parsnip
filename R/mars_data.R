
mars_arg_key <- data.frame(
  earth =      c(   "nprune",      "degree",      "pmethod"),
  stringsAsFactors = FALSE,
  row.names =  c("num_terms", "prod_degree", "prune_method")
)

mars_modes <- c("classification", "regression", "unknown")

mars_engines <- data.frame(
  earth =    rep(TRUE, 3),
  row.names =  c("classification", "regression", "unknown")
)

# ------------------------------------------------------------------------------

mars_earth_data <-
  list(
    libs = "earth",
    fit = list(
      interface = "data.frame",
      protect = c("x", "y", "weights"),
      func = c(pkg = "earth", fun = "earth"),
      defaults =
        list()
    )
  )

