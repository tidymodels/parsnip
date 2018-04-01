#' Control the fit function
#' 
#' Options can be passed to the [fit()] function that control the output and 
#'  computations
#'  
#' @param verbosity An integer where a value of zero indicates
#'  that no messages or output should be shown when packages are
#'  loaded or when the model is fit. A value of 1 means that package
#'  loading is quiet but model fits can produce output to the screen
#'  (depending on if they contain their own `verbose`-type
#'  argument). A value of 2 or more indicates that any output should
#'  be seen.
#' @param catch A logical where a value of `TRUE` will evaluate
#'  the model inside of `try(, silent = TRUE)`. If the model fails,
#'  an object is still returned (without an error) that inherits the
#'  class "try-error".
#' @return A named list with the results of the function call
#' @export
#' 

fit_control <- function(verbosity = 1L, catch = FALSE) {
  res <- list(verbosity = verbosity, catch = catch)
  res <- check_control(res)
  res
}
