#' Control the fit function
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
#' @examples
#' control_parsnip(verbosity = 2L)
#'
#' @export
control_parsnip <- function(verbosity = 1L, catch = FALSE) {
  # Any added arguments should also be added in superset control functions
  # in other packages
  res <- list(verbosity = verbosity, catch = catch)
  res <- check_control(res)
  class(res) <- "control_parsnip"
  res
}

check_control <- function(x) {
  if (!is.list(x))
    rlang::abort("control should be a named list.")
  if (!isTRUE(all.equal(sort(names(x)), c("catch", "verbosity"))))
    rlang::abort("control should be a named list with elements 'verbosity' and 'catch'.")
  # based on ?is.integer
  int_check <- function(x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol
  if (!int_check(x$verbosity))
    rlang::abort("verbosity should be an integer.")
  if (!is.logical(x$catch))
    rlang::abort("catch should be a logical.")
  x
}

#' @export
print.control_parsnip <- function(x, ...) {
  cat("parsnip control object\n")
  if (x$verbosity > 1)
    cat(" - verbose level", x$verbosity, "\n")
  if (x$catch)
    cat(" - fit errors will be caught\n")
  invisible(x)
}
