#' Succinct summary of parsnip object
#'
#' `type_sum` controls how objects are shown when inside tibble
#'  columns.
#' @param x	A `model_spec` or `model_fit` object to summarise.
#' @details For `model_spec` objects, the summary is "`spec[?]`"
#'  or "`spec[+]`". The former indicates that either the model
#'  mode has not been declared or that the specification has
#'  `tune()` parameters. Otherwise, the latter is shown.
#'
#' For fitted models, either "`fit[x]`" or "`fit[+]`" are used
#'  where the "x" implies that the model fit failed in some way.
#' @return A character value.
#' @method type_sum model_spec
#' @keywords internal
#' @export
type_sum.model_spec <- function(x) {
  resolved <- TRUE
  if (x$mode == "unknown")
    resolved <- FALSE
  arg_info <- generics::tune_args(x)
  if (any(arg_info$tunable))
    resolved <- FALSE

  res <- "spec"
  if (resolved) {
    res <- paste0(res, "[+]")
  } else {
    res <- paste0(res, "[?]")
  }
  res
}

#' @rdname type_sum.model_spec
#' @method type_sum model_fit
#' @keywords internal
#' @export
type_sum.model_fit <- function(x) {
  resolved <- TRUE
  if (inherits(x$fit, "try-error"))
    resolved <- FALSE

  res <- "fit"
  if (resolved) {
    res <- paste0(res, "[+]")
  } else {
    res <- paste0(res, "[x]")
  }
  res
}
