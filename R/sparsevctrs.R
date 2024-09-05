to_sparse_data_frame <- function(x, object) {
  if (methods::is(x, "sparseMatrix")) {
    if (allow_sparse(object)) {
      x <- sparsevctrs::coerce_to_sparse_data_frame(x)
    } else {
      cli::cli_abort(
      "{.arg x} is a sparse matrix, but {.fn {class(object)[1]}} with
       engine {.code {object$engine}} doesn't accept that.")
    }
  } else if (is.data.frame(x)) {
    x <- materialize_sparse_tibble(x, object, "x")
  }
  x
}

is_sparse_tibble <- function(x) {
  any(vapply(x, sparsevctrs::is_sparse_vector, logical(1)))
}

materialize_sparse_tibble <- function(x, object, input) {
  if (is_sparse_tibble(x) && (!allow_sparse(object))) {
    cli::cli_warn(
      "{.arg {input}} is a sparse tibble, but {.fn {class(object)[1]}} with
      engine {.code {object$engine}} doesn't accept that. Converting to 
      non-sparse."
    )
    for (i in seq_along(ncol(x))) {
      # materialize with []
      x[[i]] <- x[[i]][]
    }
  }
  x
}
