null_model_arg_key <- data.frame(
  parsnip   =  NULL,
  row.names =  NULL,
  stringsAsFactors = FALSE
)

null_model_modes <- c("classification", "regression", "unknown")

null_model_engines <- data.frame(
  parsnip   = c(TRUE, TRUE, FALSE),
  row.names =  c("classification", "regression", "unknown")
)

# ------------------------------------------------------------------------------

null_model_parsnip_data <-
  list(
    libs = "parsnip",
    fit = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(fun = "nullmodel"),
      defaults = list()
      ),
    class = list(
        pre = NULL,
        post = NULL,
        func = c(fun = "predict"),
        args =
          list(
            object = quote(object$fit),
            new_data = quote(new_data),
            type = "class"
          )
      ),
    numeric = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data),
          type = "numeric"
        )
      ),
    raw = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data),
          type = "raw"
        )
      )
    )
