#' Radial basis function support vector machines
#'
#' @description
#'
#' `svm_rbf()` defines a support vector machine model. For classification,
#' the model tries to maximize the width of the margin between classes using a
#' nonlinear class boundary. For regression, the model optimizes a robust loss
#' function that is only affected by very large model residuals and uses
#' nonlinear functions of the predictors. The function can fit classification
#' and regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("svm_rbf")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams nearest_neighbor
#' @param engine A single character string specifying what computational engine
#'  to use for fitting. Possible engines are listed below. The default for this
#'  model is `"kernlab"`.
#' @param cost A positive number for the cost of predicting a sample within
#'  or on the wrong side of the margin
#' @param rbf_sigma A positive number for radial basis function.
#' @param margin A positive number for the epsilon in the SVM insensitive
#'  loss function (regression only)
#'
#' @templateVar modeltype svm_rbf
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("svm_rbf")}
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("svm_rbf")
#'
#' svm_rbf(mode = "classification", rbf_sigma = 0.2)
#' @export

svm_rbf <-
  function(mode = "unknown", engine = "kernlab",
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
      user_specified_mode = !missing(mode),
      method = NULL,
      engine = engine,
      user_specified_engine = !missing(engine)
    )
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

    args <- list(
      cost   = enquo(cost),
      rbf_sigma  = enquo(rbf_sigma),
      margin  = enquo(margin)
    )

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "svm_rbf",
      ...
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

#' @export
check_args.svm_rbf <- function(object, call = rlang::caller_env()) {
  invisible(object)
}

# ------------------------------------------------------------------------------

svm_reg_post <- function(results, object) {
  results[,1]
}

