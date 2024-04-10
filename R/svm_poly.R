#' Polynomial support vector machines
#'
#' @description
#'
#' `svm_poly()` defines a support vector machine model. For classification,
#' the model tries to maximize the width of the margin between classes using a
#' polynomial class boundary. For regression, the model optimizes a robust loss
#' function that is only affected by very large model residuals and uses polynomial
#' functions of the predictors. This function can fit classification and
#' regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("svm_poly")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams nearest_neighbor
#' @param cost A positive number for the cost of predicting a sample within
#'  or on the wrong side of the margin
#' @param degree A positive number for polynomial degree.
#' @param scale_factor A positive number for the polynomial scaling factor.
#' @param margin A positive number for the epsilon in the SVM insensitive
#'  loss function (regression only)
#'
#' @templateVar modeltype svm_poly
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("svm_poly")}
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("svm_poly")
#'
#' svm_poly(mode = "classification", degree = 1.2)
#' @export

svm_poly <-
  function(mode = "unknown", engine = "kernlab",
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
      user_specified_mode = !missing(mode),
      method = NULL,
      engine = engine,
      user_specified_engine = !missing(engine)
    )
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

    args <- list(
      cost   = enquo(cost),
      degree  = enquo(degree),
      scale_factor  = enquo(scale_factor),
      margin  = enquo(margin)
    )

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "svm_poly",
      ...
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

#' @export
check_args.svm_poly <- function(object, call = rlang::caller_env()) {
  invisible(object)
}

# ------------------------------------------------------------------------------

svm_reg_post <- function(results, object) {
  results[,1]
}

