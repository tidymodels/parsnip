to_sparse_data_frame <- function(x, object) {
  if (methods::is(x, "sparseMatrix")) {
    if (allow_sparse(object)) {
      x <- sparsevctrs::coerce_to_sparse_data_frame(x)
    } else {
      cli::cli_warn(c(
        "!" = "{.arg x} is a sparse matrix, but model doesn't accept that.",
        "i" = "Converted {.arg x} to data.frame."
      ))
      x <- as.data.frame(x)
    }
  }
  x
}