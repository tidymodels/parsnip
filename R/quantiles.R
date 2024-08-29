# Helpers for quantile regression models

check_quantile_level <- function(x, object, call) {
  if ( object$mode != "quantile regression" ) {
    return(invisible(TRUE))
  } else {
    if ( is.null(x) ) {
      cli::cli_abort("In {.fn check_mode}, at least one value of
      {.arg quantile_level} must be specified for quantile regression models.")
    }
  }
  # TODO we need better vectorization here, otherwise we get things like:
  # "Error during wrapup: i In index: 2." in the traceback.
  res <-
    purrr::map(x,
               ~ check_number_decimal(.x, min = 0, max = 1,
                                      arg = "quantile_level", call = call,
                                      allow_infinite = FALSE)
  )
  return(invisible(TRUE))
}


