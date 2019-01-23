nullmodel_arg_key <- data.frame(
  parsnip   =  NULL,
  row.names =  NULL,
  stringsAsFactors = FALSE
)

nullmodel_modes <- c("classification", "regression", "unknown")

nullmodel_engines <- data.frame(
  parsnip   = c(TRUE, TRUE, FALSE),
  row.names =  c("classification", "regression", "unknown")
)

# ------------------------------------------------------------------------------

nullmodel_parsnip_data <-
  list(
    libs = "parsnip",
    fit = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(fun = "null_model"),
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
          type = "raw"
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
