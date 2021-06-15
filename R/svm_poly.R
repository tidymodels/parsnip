#' Polynomial support vector machines
#'
#' @description
#'
#' `svm_poly()` defines a support vector machine model. For classification,
#' these models try to maximize the width of the margin between classes.
#' For regression, the model optimizes a robust loss function that is only
#' affected by very large model residuals.
#'
#' This SVM model uses a nonlinear function, defined by a polynomial function,
#' to create the decision boundary or regression line.
#'
#' There are different ways to fit this model. See the engine-specific pages 
#' for more details:
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("svm_poly")}
#'
#' More information on how `parsnip` is used for modeling is at
#' \url{https://www.tidymodels.org}.
#'
#' @inheritParams boost_tree
#' @param cost A positive number for the cost of predicting a sample within
#'  or on the wrong side of the margin
#' @param degree A positive number for polynomial degree.
#' @param scale_factor A positive number for the polynomial scaling factor.
#' @param margin A positive number for the epsilon in the SVM insensitive
#'  loss function (regression only)
#'
#' @template spec-details
#'
#' @template spec-references
#' 
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("svm_poly")}
#'
#' @examples
#' show_engines("svm_poly")
#'
#' svm_poly(mode = "classification", degree = 1.2)
#' @export

svm_poly <-
  function(mode = "unknown",
           cost = NULL, degree = NULL, scale_factor = NULL, margin = NULL) {

    args <- list(
      cost   = enquo(cost),
      degree  = enquo(degree),
      scale_factor  = enquo(scale_factor),
      margin = enquo(margin)
    )

    new_model_spec(
      "svm_poly",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.svm_poly <- function(x, ...) {
  cat("Polynomial Support Vector Machine Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if(!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

#' @method update svm_poly
#' @rdname parsnip_update
#' @export
update.svm_poly <-
  function(object,
           parameters = NULL,
           cost = NULL, degree = NULL, scale_factor = NULL, margin = NULL,
           fresh = FALSE,
           ...) {

    eng_args <- update_engine_parameters(object$eng_args, ...)

    if (!is.null(parameters)) {
      parameters <- check_final_param(parameters)
    }

    args <- list(
      cost   = enquo(cost),
      degree  = enquo(degree),
      scale_factor  = enquo(scale_factor),
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
      "svm_poly",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

#' @export
translate.svm_poly <- function(x, engine = x$engine, ...) {
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

    # convert degree and scale to a `kpar` argument.
    if (any(arg_names %in% c("degree", "scale", "offset"))) {
      kpar <- expr(list())
      if (any(arg_names == "degree")) {
        kpar$degree <- arg_vals$degree
        arg_vals$degree <- NULL
      }
      if (any(arg_names == "scale")) {
        kpar$scale <- arg_vals$scale
        arg_vals$scale <- NULL
      }
      if (any(arg_names == "offset")) {
        kpar$offset <- arg_vals$offset
        arg_vals$offset <- NULL
      }
      arg_vals$kpar <- kpar
    }

  }
  x$method$fit$args <- arg_vals

  # worried about people using this to modify the specification
  x
}

# ------------------------------------------------------------------------------

check_args.svm_poly <- function(object) {
  invisible(object)
}

# ------------------------------------------------------------------------------

svm_reg_post <- function(results, object) {
  results[,1]
}

