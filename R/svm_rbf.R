#' General interface for radial basis function support vector machines
#'
#' `svm_rbf()` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R or via Spark. The main arguments for the
#'  model are:
#' \itemize{
#'   \item \code{cost}: The cost of predicting a sample within or on the
#'    wrong side of the margin.
#'   \item \code{rbf_sigma}: The precision parameter for the radial basis
#'     function.
#'   \item \code{margin}: The epsilon in the SVM insensitive loss function
#'    (regression only)
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using `set_engine()`. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions. If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#' @inheritParams boost_tree
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#' @param cost A positive number for the cost of predicting a sample within
#'  or on the wrong side of the margin
#' @param rbf_sigma A positive number for radial basis function.
#' @param margin A positive number for the epsilon in the SVM insensitive
#'   loss function (regression only)
#' @details
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"kernlab"` (the default)
#' \item \pkg{R}:  `"liquidSVM"`
#' }
#'
#'
#' @includeRmd man/rmd/svm-rbf.Rmd details
#'
#' @importFrom purrr map_lgl
#' @seealso [fit()]
#' @examples
#' svm_rbf(mode = "classification", rbf_sigma = 0.2)
#' # Parameters can be represented by a placeholder:
#' svm_rbf(mode = "regression", cost = varying())
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

#' @export
#' @inheritParams update.boost_tree
#' @param object A radial basis function SVM model specification.
#' @examples
#' model <- svm_rbf(cost = 10, rbf_sigma = 0.1)
#' model
#' update(model, cost = 1)
#' update(model, cost = 1, fresh = TRUE)
#' @method update svm_rbf
#' @rdname svm_rbf
#' @export
update.svm_rbf <-
  function(object,
           parameters = NULL,
           cost = NULL, rbf_sigma = NULL, margin = NULL,
           fresh = FALSE,
           ...) {
    update_dot_check(...)

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
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
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

