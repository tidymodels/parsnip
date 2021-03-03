#' General interface for linear support vector machines
#'
#' `svm_linear()` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R or via Spark. The main arguments for the
#'  model are:
#' \itemize{
#'   \item \code{cost}: The cost of predicting a sample within or on the
#'    wrong side of the margin.
#'   \item \code{margin}: The epsilon in the SVM insensitive loss function
#'    (regression only)
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and arguments can be
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
#' @param margin A positive number for the epsilon in the SVM insensitive
#'  loss function (regression only)
#' @details
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"LiblineaR"` (the default) or `"kernlab"`
#' }
#'
#'
#' @includeRmd man/rmd/svm-linear.Rmd details
#'
#' @importFrom purrr map_lgl
#' @seealso [fit()]
#' @examples
#' show_engines("svm_linear")
#'
#' svm_linear(mode = "classification")
#' # Parameters can be represented by a placeholder:
#' svm_linear(mode = "regression", cost = varying())
#' @export

svm_linear <-
  function(mode = "unknown",
           cost = NULL, margin = NULL) {

    args <- list(
      cost   = enquo(cost),
      margin = enquo(margin)
    )

    new_model_spec(
      "svm_linear",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.svm_linear <- function(x, ...) {
  cat("Linear Support Vector Machine Specification (", x$mode, ")\n\n", sep = "")
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
#' @param object A linear SVM model specification.
#' @examples
#' model <- svm_linear(cost = 3)
#' model
#' update(model, cost = 1)
#' update(model, cost = 1, fresh = TRUE)
#' @method update svm_linear
#' @rdname svm_linear
#' @export
update.svm_linear <-
  function(object,
           parameters = NULL,
           cost = NULL, margin = NULL,
           fresh = FALSE,
           ...) {

    eng_args <- update_engine_parameters(object$eng_args, ...)

    if (!is.null(parameters)) {
      parameters <- check_final_param(parameters)
    }

    args <- list(
      cost   = enquo(cost),
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
      "svm_linear",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

#' @export
translate.svm_linear <- function(x, engine = x$engine, ...) {
  x <- translate.default(x, engine = engine, ...)

  # slightly cleaner code using
  arg_vals <- x$method$fit$args
  arg_names <- names(arg_vals)

  # add checks to error trap or change things for this method

  if (x$engine == "LiblineaR") {

    if (is_null(x$eng_args$type)) {
      liblinear_type <- NULL
    } else {
      liblinear_type <- quo_get_expr(x$eng_args$type)
    }

    if (x$mode == "regression") {
      if (is_null(quo_get_expr(x$args$margin)))
        arg_vals$svr_eps <- 0.1
      if (!is_null(liblinear_type))
        if(!liblinear_type %in% 11:13)
          rlang::abort(
            paste0("The LiblineaR engine argument of `type` = ",
                   liblinear_type,
                   " does not correspond to an SVM regression model.")
          )
    } else if (x$mode == "classification") {
      if (!is_null(liblinear_type))
        if(!liblinear_type %in% 1:5)
          rlang::abort(
            paste0("The LiblineaR engine argument of `type` = ",
                   liblinear_type,
                   " does not correspond to an SVM classification model.")
          )
    }
  }

  if (x$engine == "kernlab") {

    # unless otherwise specified, classification models predict probabilities
    if (x$mode == "classification" && !any(arg_names == "prob.model"))
      arg_vals$prob.model <- TRUE
    if (x$mode == "classification" && any(arg_names == "epsilon"))
      arg_vals$epsilon <- NULL

  }

  x$method$fit$args <- arg_vals

  # worried about people using this to modify the specification
  x
}

# ------------------------------------------------------------------------------

check_args.svm_linear <- function(object) {
  invisible(object)
}

# ------------------------------------------------------------------------------

svm_linear_post <- function(results, object) {
  results$predictions
}

svm_reg_linear_post <- function(results, object) {
  results[,1]
}

