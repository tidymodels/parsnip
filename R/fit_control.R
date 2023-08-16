#' Control the fit function
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Pass options to the [fit.model_spec()] function to control its
#' output and computations
#'
#' @param verbosity An integer to control how verbose the output is. For a
#'  value of zero, no messages or output are shown when packages are loaded or
#'  when the model is fit. For a value of 1, package loading is quiet but model
#'  fits can produce output to the screen (depending on if they contain their
#'  own `verbose`-type argument). For a value of 2 or more, any output at all
#'  is displayed and the execution time of the fit is recorded and printed.
#' @param catch A logical where a value of `TRUE` will evaluate the model
#'  inside of `try(, silent = TRUE)`. If the model fails, an object is still
#'  returned (without an error) that inherits the class "try-error".
#' @return An S3 object with class "control_parsnip" that is a named list
#' with the results of the function call
#'
#' @examplesIf !parsnip:::is_cran_check()
#' fit_control(verbosity = 2L)
#'
#' @details
#' `fit_control()` is deprecated in favor of `control_parsnip()`.
#'
#' @export
#' @keywords internal
fit_control <- function(verbosity = 1L, catch = FALSE) {
  lifecycle::deprecate_warn("0.1.8", "fit_control()", "control_parsnip()")
  control_parsnip(verbosity = verbosity, catch = catch)
}
