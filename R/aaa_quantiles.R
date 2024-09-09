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
  x <- sort(unique(x))
  # TODO we need better vectorization here, otherwise we get things like:
  # "Error during wrapup: i In index: 2." in the traceback.
  res <-
    purrr::map(x,
               ~ check_number_decimal(.x, min = 0, max = 1,
                                      arg = "quantile_level", call = call,
                                      allow_infinite = FALSE)
  )
  x
}

# Assumes the columns have the same order as quantile_level
restructure_rq_pred <- function(x, object) {
  num_quantiles <- NCOL(x)
  if ( num_quantiles == 1L ){
    x <- matrix(x, ncol = 1)
  }
  n <- nrow(x)

  quantile_level <- object$spec$quantile_level
  res <-
    tibble::tibble(
    .pred_quantile = as.vector(x),
    .quantile_level = rep(quantile_level, each = n),
    .row = rep(1:n, num_quantiles))
  res <- vctrs::vec_split(x = res[,1:2], by = res[, ".row"])
  res <- vctrs::vec_cbind(res$key, tibble::new_tibble(list(.pred_quantile = res$val)))
  res$.row <- NULL
  res
}

