#' Determine required packages for a model
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @param x A model specification or fit.
#' @param ... Not used.
#' @return A character string of package names (if any).
#' @details
#' For a model specification, the engine must be set. The list produced by
#' `req_pkgs()`does not include the `parsnip` package while `required_pkgs()`
#' does.
#' @examples
#' should_fail <- try(req_pkgs(linear_reg()), silent = TRUE)
#' should_fail
#'
#' linear_reg() %>%
#'   set_engine("glmnet") %>%
#'   req_pkgs()
#'
#' linear_reg() %>%
#'   set_engine("lm") %>%
#'   fit(mpg ~ ., data = mtcars) %>%
#'   req_pkgs()
#' @export
req_pkgs <- function(x, ...) {
  lifecycle::deprecate_soft("0.1.8", "req_pkgs()", "required_pkgs()")
  UseMethod("req_pkgs")
}

#' @export
#' @rdname req_pkgs
req_pkgs.model_spec <- function(x, ...) {
  if (is.null(x$engine)) {
    rlang::abort("Please set an engine.")
  }
  setdiff(get_pkgs(x, FALSE), "parsnip")
}

#' @export
#' @rdname req_pkgs
req_pkgs.model_fit <- function(x, ...) {
  setdiff(get_pkgs(x$spec, FALSE), "parsnip")
}
