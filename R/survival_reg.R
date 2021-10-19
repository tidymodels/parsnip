#' Parametric survival regression
#'
#' `survival_reg()` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  R. The main argument for the
#'  model is:
#' \itemize{
#'   \item \code{dist}: The probability distribution of the outcome.
#' }
#' This argument is converted to its specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using `set_engine()`. If left to its default
#'  here (`NULL`), the value is taken from the underlying model
#'  functions. If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#' @param mode A single character string for the prediction outcome mode.
#'  The only possible value for this model is "censored regression".
#' @param engine A single character string specifying what computational engine
#'  to use for fitting. Possible engines are listed below. The default for this
#'  model is `"survival"`.
#' @param dist A character string for the outcome distribution. "weibull" is
#'  the default.
#' @details
#' The data given to the function are not saved and are only used
#' to determine the _mode_ of the model. For `survival_reg()`,the
#' mode will always be "censored regression".
#'
#' Since survival models typically involve censoring (and require the use of
#' [survival::Surv()] objects), the [fit.model_spec()] function will require that the
#' survival model be specified via the formula interface.
#'
#' @seealso [fit.model_spec()], [survival::Surv()], [set_engine()], [update()]
#' @examples
#' survival_reg()
#' @keywords internal
#' @export
survival_reg <- function(mode = "censored regression", engine = "survival", dist = NULL) {

  args <- list(
    dist = enquo(dist)
  )

  new_model_spec(
    "survival_reg",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = engine
  )
}

#' @export
print.survival_reg <- function(x, ...) {
  cat("Parametric Survival Regression Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @method update survival_reg
#' @rdname parsnip_update
#' @export
update.survival_reg <- function(object, parameters = NULL, dist = NULL, fresh = FALSE, ...) {

  eng_args <- update_engine_parameters(object$eng_args, ...)

  if (!is.null(parameters)) {
    parameters <- check_final_param(parameters)
  }

  args <- list(
    dist = enquo(dist)
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
    "survival_reg",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}


#' @export
translate.survival_reg <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'survival'` for translation.")
    engine <- "survival"
  }
  x <- translate.default(x, engine, ...)
  x
}


check_args.survival_reg <- function(object) {

  if (object$engine == "flexsurv") {

    args <- lapply(object$args, rlang::eval_tidy)

    # `dist` has no default in the function
    if (all(names(args) != "dist") || is.null(args$dist))
      object$args$dist <- "weibull"
  }

  invisible(object)
}
