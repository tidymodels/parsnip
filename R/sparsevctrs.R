to_sparse_data_frame <- function(x, object) {
  if (methods::is(x, "sparseMatrix")) {
    if (allow_sparse(object)) {
      x <- sparsevctrs::coerce_to_sparse_data_frame(x)
    } else {
      cli::cli_abort(
      "{.arg x} is a sparse matrix, but {.fn {class(object)[1]}} with
       engine {.code {object$engine}} doesn't accept that.")
    }
  }
  x
}
