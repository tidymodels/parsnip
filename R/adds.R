#' Add a column of row numbers to a data frame
#'
#' @param x A data frame
#' @return The same data frame with a column of 1-based integers named `.row`.
#' @examples
#' mtcars %>% add_rowindex()
#' @export
#' @importFrom dplyr mutate
add_rowindex <- function(x) {
  if (!is.data.frame(x)) {
    stop("`x` should be a data frame.", call. = FALSE)
  }
  if (nrow(x) > 0) {
    x <- dplyr::mutate(x, .row = 1:nrow(x))
  } else {
    x$.row <- integer(0)
  }
  x
}

