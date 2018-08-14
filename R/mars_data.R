
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

###################################################################

mars_earth_data <-
  list(
    libs = "earth",
    fit = list(
      interface = "data.frame",
      protect = c("x", "y", "weights"),
      func = c(pkg = "earth", fun = "earth"),
      defaults =
        list()
    ),
    pred = list(
      pre = NULL,
      post = maybe_multivariate,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(newdata),
          type = "response"
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
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(newdata)
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
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(newdata)
        )
    )
  )

