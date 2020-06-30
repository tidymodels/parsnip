#' Contrast function for one-hot encodings
#'
#' This contrast function produces a model matrix that has indicator columns for
#' each level of each factor.
#'
#' @param n A vector of character factor levels or the number of unique levels.
#' @param contrasts This argument is for backwards compatibility and only the
#'   default of `TRUE` is supported.
#' @param sparse This argument is for backwards compatibility and only the
#'   default of `FALSE` is supported.
#'
#' @return A diagonal matrix that is `n`-by-`n`.
#'
#' @export
contr_one_hot <- function(n, contrasts = TRUE, sparse = FALSE) {
  if (sparse) {
    rlang::warn("`sparse = TRUE` not implemented for `contr_one_hot()`.")
  }

  if (!contrasts) {
    rlang::warn("`contrasts = FALSE` not implemented for `contr_one_hot()`.")
  }

  if (is.character(n)) {
    names <- n
    n <- length(names)
  } else if (is.numeric(n)) {
    n <- as.integer(n)

    if (length(n) != 1L) {
      rlang::abort("`n` must have length 1 when an integer is provided.")
    }

    names <- as.character(seq_len(n))
  } else {
    rlang::abort("`n` must be a character vector or an integer of size 1.")
  }

  out <- diag(n)

  rownames(out) <- names
  colnames(out) <- names

  out
}
