#' Resolve a Model Specification for a Computational Engine
#'
#' `translate` will translate a model specification into a code
#'  object that is specific to a particular engine (e.g. R package).
#'  It translates generic parameters to their counterparts.
#'
#' @param x A model specification.
#' @param ... Not currently used.
#' @export

translate <- function (x, ...)
  UseMethod("translate")

#' @importFrom utils getFromNamespace
#' @importFrom purrr list_modify
#' @export
translate.default <- function(x, engine, ...) {
  check_empty_ellipse(...)
  x$engine <- engine
  x <- check_engine(x)

  # check for installs

  x$method <- get_model_fit_info(x, x$engine)

  arg_key <- getFromNamespace(
    paste0(specifc_model(x), "_arg_key"),
    ns = "parsnip"
    )

  # deharmonize primary arguments
  actual_args <- deharmonize(x$args, arg_key, x$engine)

  # check secondary arguments to see if they are in the final
  # expression unless there are dots, warn if protected args are
  # being altered
  x$others <- check_others(x$others, x$method)

  # keep only modified args
  modifed_args <- !vapply(actual_args, null_value, lgl(1))
  actual_args <- actual_args[modifed_args]

  # look for alternates if not modified in other
  if(length(x$method$alternates) > 0) {
    in_other <- names(x$method$alternates) %in% names(x$others)
    x$alternates <- x$method$alternates[!in_other]
  }

  # combine primary, others, and alternates
  protected <- lapply(x$method$protect, function(x) expr(missing_arg()))
  names(protected) <- x$method$protect

  x$method$fit_args <- c(protected, actual_args, x$others, x$alternates)

  x$method <- reorder_args(x$method)

  # put in correct order
  x
}
