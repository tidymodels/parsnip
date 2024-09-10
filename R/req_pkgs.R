#' Determine required packages for a model
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @param x A [model specification][model_spec] or [fit][model_fit].
#' @param ... Not used.
#' @return A character string of package names (if any).
#' @details
#' This function has been deprecated in favor of `required_pkgs()`.
#'
#' @export
req_pkgs <- function(x, ...) {
  lifecycle::deprecate_stop("0.1.8", "req_pkgs()", "required_pkgs()")
}
