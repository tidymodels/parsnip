
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
      defaults = list(keepxy = TRUE)
    ),
    numeric = list(
      pre = NULL,
      post = maybe_multivariate,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "response"
        )
    ),
    class = list(
      pre = NULL,
      post = function(x, object) {
        x <- ifelse(x[,1] >= 0.5, object$lvl[2], object$lvl[1])
        x
      },
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "response"
        )
    ),
    classprob = list(
      pre = NULL,
      post = function(x, object) {
        x <- x[,1]
        x <- tibble(v1 = 1 - x, v2 = x)
        colnames(x) <- object$lvl
        x
      },
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "response"
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

