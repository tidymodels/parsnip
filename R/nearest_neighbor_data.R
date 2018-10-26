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
    ),
    numeric = list(
      # seems unnecessary here as the predict_numeric catches it based on the
      # model mode
      pre = function(x, object) {
        if (object$fit$response != "continuous") {
          stop("`kknn` model does not appear to use numeric predictions. Was ",
               "the model fit with a continuous response variable?",
               call. = FALSE)
        }
        x
      },
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "raw"
        )
    ),
    class = list(
      pre = function(x, object) {
        if (!(object$fit$response %in% c("ordinal", "nominal"))) {
          stop("`kknn` model does not appear to use class predictions. Was ",
               "the model fit with a factor response variable?",
               call. = FALSE)
        }
        x
      },
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "raw"
        )
    ),
    classprob = list(
      pre = function(x, object) {
        if (!(object$fit$response %in% c("ordinal", "nominal"))) {
          stop("`kknn` model does not appear to use class predictions. Was ",
               "the model fit with a factor response variable?",
               call. = FALSE)
        }
        x
      },
      post = function(result, object) as_tibble(result),
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
