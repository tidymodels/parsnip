#' Contrast function for one-hot encodings
#'
#' This contrast function produces a model matrix with indicator columns for
#' each level of each factor.
#'
#' @param n A vector of character factor levels (of length >=1) or the number
#' of unique levels (>= 1).
#' @param contrasts This argument is for backwards compatibility and only the
#'   default of `TRUE` is supported.
#' @param sparse This argument is for backwards compatibility and only the
#'   default of `FALSE` is supported.
#'
#' @includeRmd man/rmd/one-hot.md details
#'
#' @return A diagonal matrix that is `n`-by-`n`.
#'
#' @export
contr_one_hot <- function(n, contrasts = TRUE, sparse = FALSE) {
  if (sparse) {
    cli::cli_warn("{.code sparse = TRUE} not implemented for {.fun contr_one_hot}.")
  }

  if (!contrasts) {
    cli::cli_warn("{.code contrasts = FALSE} not implemented for {.fun contr_one_hot}.")
  }

  if (is.character(n)) {
    if (length(n) < 1) {
      cli::cli_abort("{.arg n} cannot be empty.")
    }
    names <- n
    n <- length(names)
  } else if (is.numeric(n)) {
    check_number_whole(n, min = 1)
    n <- as.integer(n)

    if (length(n) != 1L) {
      cli::cli_abort("{.arg n} must have length 1 when an integer is provided.")
    }

    names <- as.character(seq_len(n))
  } else {
    check_number_whole(n, min = 1)
  }

  out <- diag(n)

  rownames(out) <- names
  colnames(out) <- names

  out
}
