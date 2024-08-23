#' tidy methods for LiblineaR models
#'
#' `tidy()` methods for the various `LiblineaR` models that return the
#' coefficients from the parsnip model fit.
#' @param x A fitted parsnip model that used the `LiblineaR` engine.
#' @param ... Not used
#' @return A tibble with columns `term` and `estimate`.
#' @keywords internal
#' @export

tidy._LiblineaR <- function(x, ...) {
  check_installs(x$spec)
  ret <- tibble(colnames(x$fit$W), x$fit$W[1,])
  colnames(ret) <- c("term", "estimate")

  ret
}
