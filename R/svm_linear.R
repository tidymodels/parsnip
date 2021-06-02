#' Linear support vector machines
#'
#' @description
#'
#' `svm_linear()` defines a support vector machine model. For classification,
#' these models try to maximize the width of the margin between classes.
#' For regression, the model optimizes a robust loss function that is only
#' affected by very large model residuals.
#'
#' This SVM model uses a linear function to create the decision boundary or
#' regression line.
#'
#' There are different ways to fit this model. See the engine-specific pages 
#' for more details:
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::find_engine_files("svm_linear")}
#'
#' More information on how `parsnip` is used for modeling is at
#' \url{https://www.tidymodels.org}.
#'
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#' @param cost A positive number for the cost of predicting a sample within
#'  or on the wrong side of the margin
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
#' \code{\link[=details_svm_linear_LiblineaR]{LiblineaR engine details}},
#' \code{\link[=details_svm_linear_kernlab]{kernlab engine details}}
#' @examples
#' show_engines("svm_linear")
#'
#' svm_linear(mode = "classification")
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

#' @method update svm_linear
#' @rdname parsnip_update
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

