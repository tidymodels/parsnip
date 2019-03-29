svm_rbf_arg_key <- data.frame(
  kernlab   =  c(   "C",     "sigma", "epsilon"),
  liquidSVM =  c(   "C",     "sigma", "epsilon"),
  row.names =  c("cost", "rbf_sigma",  "margin"),
  stringsAsFactors = FALSE
)

svm_rbf_modes <- c("classification", "regression", "unknown")

svm_rbf_engines <- data.frame(
  kernlab   =  c(TRUE, TRUE, FALSE),
  liquidSVM = c(TRUE, TRUE, FALSE),
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


svm_rbf_liquidSVM_data <-
  list(
    libs = "liquidSVM",
    fit = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(pkg = "liquidSVM", fun = "svm"),
      defaults = list()
    ),
    numeric = list(
      pre = NULL,
      post = function(results, object) {results},
      func = c(pkg = NULL, fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    ),
    class = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = NULL, fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    ),
    classprob = list(
      pre = function(x, object) {
        if (object$fit$predict.prob == FALSE)
          stop("`svm` model does not appear to use class probabilities. Was ",
               "the model fit with `predict.prob = TRUE`?",
               call. = FALSE)
        x
      },
      post = function(result, object) {
        res <- as_tibble(result)
        names(res) <- object$lvl
        res
        },
      func = c(pkg = NULL, fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          predict.prob = TRUE
        )
    ),
    raw = list(
      pre = NULL,
      func = c(pkg = NULL, fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )
