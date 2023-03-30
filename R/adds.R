#' Add a column of row numbers to a data frame
#'
#' @param x A data frame
#' @return The same data frame with a column of 1-based integers named `.row`.
#' @examples
#' mtcars %>% add_rowindex()
#' @export
add_rowindex <- function(x) {
  if (!is.data.frame(x)) {
    rlang::abort("`x` should be a data frame.")
  }
  x <- dplyr::mutate(x, .row = seq_len(nrow(x)))
  x
}

