#' Condense control object into strictly smaller control object
#'
#' This function is used to help the hierarchy of control functions used
#' throughout the tidymodels packages. It is now assumed that each control
#' function is either a subset or a superset of another control function.
#'
#' @param x A control object to be condensed.
#' @param ref A control object that is used to determine what element should be
#'   kept.
#'
#' @return A control object with the same elements and classes of `ref`, with
#'   values of `x`.
#' @keywords internal
#' @export
#'
#' @examplesIf !parsnip:::is_cran_check()
#' ctrl <- control_parsnip(catch = TRUE)
#' ctrl$allow_par <- TRUE
#' str(ctrl)
#'
#' ctrl <- condense_control(ctrl, control_parsnip())
#' str(ctrl)
condense_control <- function(x, ref) {
  mismatch <- setdiff(names(ref), names(x))
  if (length(mismatch)) {
    cli::cli_abort(
      c(
        "Object of class {.cls class(x)[1]} cannot be coerced to
         object of class {.cls class(ref)[1]}.",
        "i" = "{cli::qty(mismatch)} The argument{?s} {.arg {mismatch}}
               {?is/are} missing."
      )
    )
  }
  res <- x[names(ref)]
  class(res) <- class(ref)
  res
}
