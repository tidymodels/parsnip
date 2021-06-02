#' Radial basis function support vector machines
#'
#' @description
#'
#' `svm_rbf()` defines a support vector machine model. For classification,
#' these models try to maximize the width of the margin between classes.
#' For regression, the model optimizes a robust loss function that is only
#' affected by very large model residuals.
#'
#' This SVM model uses a nonlinear function, defined by the radial basis function,
#' to create the decision boundary or regression line.
#'
#' There are different ways to fit this model. See the engine-specific pages 
#' for more details:
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::find_engine_files("svm_rbf")}
#'
#' More information on how `parsnip` is used for modeling is at
#' \url{https://www.tidymodels.org}.
#'
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#' @param cost A positive number for the cost of predicting a sample within
#'  or on the wrong side of the margin
#' @param rbf_sigma A positive number for radial basis function.
#' @param margin A positive number for the epsilon in the SVM insensitive
#'  loss function (regression only)
#' @details
#' This function only defines what _type_ of model is being fit. Once an engine
#'  is specified, the _method_ to fit the model is also defined.
#'
#' The model is not trained or fit until the [fit.model_spec()] function is used
#' with the data.
#'
#' @references \url{https://www.tidymodels.org},
#' [_Tidy Models with R_](https://tmwr.org)
#' @seealso [fit.model_spec()], [set_engine()], [update()],
#' \code{\link[=details_svm_rbf_kernlab]{kernlab engine details}}
#' @examples
#' show_engines("svm_rbf")
#'
#' svm_rbf(mode = "classification", rbf_sigma = 0.2)
#' @export

svm_rbf <-
  function(mode = "unknown",
           cost = NULL, rbf_sigma = NULL, margin = NULL) {

    args <- list(
      cost   = enquo(cost),
      rbf_sigma  = enquo(rbf_sigma),
      margin = enquo(margin)
    )

    new_model_spec(
      "svm_rbf",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.svm_rbf <- function(x, ...) {
  cat("Radial Basis Function Support Vector Machine Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if(!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

#' @method update svm_rbf
#' @rdname parsnip_update
#' @export
update.svm_rbf <-
  function(object,
           parameters = NULL,
           cost = NULL, rbf_sigma = NULL, margin = NULL,
           fresh = FALSE,
           ...) {

    eng_args <- update_engine_parameters(object$eng_args, ...)

    if (!is.null(parameters)) {
      parameters <- check_final_param(parameters)
    }

    args <- list(
      cost   = enquo(cost),
      rbf_sigma  = enquo(rbf_sigma),
      margin  = enquo(margin)
    )

    args <- update_main_parameters(args, parameters)

    if (fresh) {
      object$args <- args
      object$eng_args <- eng_args
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
      if (length(eng_args) > 0)
        object$eng_args[names(eng_args)] <- eng_args
    }

    new_model_spec(
      "svm_rbf",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

#' @export
translate.svm_rbf <- function(x, engine = x$engine, ...) {
  x <- translate.default(x, engine = engine, ...)

  # slightly cleaner code using
  arg_vals <- x$method$fit$args
  arg_names <- names(arg_vals)

  # add checks to error trap or change things for this method
  if (x$engine == "kernlab") {

    # unless otherwise specified, classification models predict probabilities
    if (x$mode == "classification" && !any(arg_names == "prob.model"))
      arg_vals$prob.model <- TRUE
    if (x$mode == "classification" && any(arg_names == "epsilon"))
      arg_vals$epsilon <- NULL

    # convert sigma and scale to a `kpar` argument.
    if (any(arg_names == "sigma")) {
      kpar <- expr(list())
      kpar$sigma <- arg_vals$sigma
      arg_vals$sigma <- NULL
      arg_vals$kpar <- kpar
    }

  }

  if (x$engine == "liquidSVM") {
    # convert parameter arguments
    if (any(arg_names == "sigma")) {
      arg_vals$gammas <- rlang::quo(1 / !!sqrt(arg_vals$sigma))
      arg_vals$sigma <- NULL
    }

    if (any(arg_names == "C")) {
      arg_vals$lambdas <- arg_vals$C
      arg_vals$C <- NULL
    }

  }

  x$method$fit$args <- arg_vals

  # worried about people using this to modify the specification
  x
}

# ------------------------------------------------------------------------------

check_args.svm_rbf <- function(object) {
  invisible(object)
}

# ------------------------------------------------------------------------------

svm_reg_post <- function(results, object) {
  results[,1]
}

