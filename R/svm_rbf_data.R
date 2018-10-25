svm_rbf_arg_key <- data.frame(
  kernlab   =  c(   "C",     "sigma", "epsilon"),
  row.names =  c("cost", "rbf_sigma",  "margin"),
  stringsAsFactors = FALSE
)

svm_rbf_modes <- c("classification", "regression", "unknown")

svm_rbf_engines <- data.frame(
  kernlab   =  c(TRUE, TRUE, FALSE),
  row.names =  c("classification", "regression", "unknown")
)

# ------------------------------------------------------------------------------

svm_rbf_kernlab_data <-
  list(
    libs = "kernlab",
    fit = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(pkg = "kernlab", fun = "ksvm"),
      defaults = list(
        kernel = "rbfdot"
      )
    ),
    numeric = list(
      pre = NULL,
      post = svm_reg_post,
      func = c(pkg = "kernlab", fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "response"
        )
    ),
    class = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "kernlab", fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "response"
        )
    ),
    classprob = list(
      pre = NULL,
      post = function(result, object) as_tibble(result),
      func = c(pkg = "kernlab", fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "probabilities"
        )
    ),
    raw = list(
      pre = NULL,
      func = c(pkg = "kernlab", fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )
