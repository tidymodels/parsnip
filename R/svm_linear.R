#' Linear support vector machines
#'
#' @description
#'
#' `svm_linear()` defines a support vector machine model. For classification,
#' the model tries to maximize the width of the margin between classes (using a
#' linear class boundary). For regression, the model optimizes a robust loss
#' function that is only affected by very large model residuals and uses a
#' linear fit. This function can fit classification and regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("svm_linear")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams nearest_neighbor
#' @param cost A positive number for the cost of predicting a sample within
#'  or on the wrong side of the margin
#' @param margin A positive number for the epsilon in the SVM insensitive
#'  loss function (regression only)
#'
#' @templateVar modeltype svm_linear
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("svm_linear")}
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("svm_linear")
#'
#' svm_linear(mode = "classification")
#' @export

svm_linear <-
  function(mode = "unknown", engine = "LiblineaR",
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
      user_specified_mode = !missing(mode),
      method = NULL,
      engine = engine,
      user_specified_engine = !missing(engine)
    )
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

    args <- list(
      cost   = enquo(cost),
      margin  = enquo(margin)
    )

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "svm_linear",
      ...
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
          cli::cli_abort(
            "The LiblineaR engine argument {.code type = {liblinear_type}}
             does not correspond to an SVM regression model."
          )
    } else if (x$mode == "classification") {
      if (!is_null(liblinear_type))
        if (!liblinear_type %in% 1:5) {
          cli::cli_abort(
            "The LiblineaR engine argument of {.code type = {liblinear_type}}
             does not correspond to an SVM classification model."
          )
        }
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

#' @export
check_args.svm_linear <- function(object, call = rlang::caller_env()) {
  invisible(object)
}

# ------------------------------------------------------------------------------

svm_linear_post <- function(results, object) {
  results$predictions
}

svm_reg_linear_post <- function(results, object) {
  results[,1]
}

