to_sparse_data_frame <- function(x, object, call = rlang::caller_env()) {
  if (is_sparse_matrix(x)) {
    if (allow_sparse(object)) {
      x <- sparsevctrs::coerce_to_sparse_data_frame(x)
    } else {
      if (inherits(object, "model_fit")) {
        object <- object$spec
      }
    
      cli::cli_abort(
        "{.arg x} is a sparse matrix, but {.fn {class(object)[1]}} with
        engine {.val {object$engine}} doesn't accept that.",
        call = call
      )
    }
  } else if (is.data.frame(x)) {
    x <- materialize_sparse_tibble(x, object, "x")
  }
  x
}

is_sparse_tibble <- function(x) {
  any(vapply(x, sparsevctrs::is_sparse_vector, logical(1)))
}

is_sparse_matrix <- function(x) {
  methods::is(x, "sparseMatrix")
}

materialize_sparse_tibble <- function(x, object, input) {
  if (is_sparse_tibble(x) && (!allow_sparse(object))) {
    if (inherits(object, "model_fit")) {
      object <- object$spec
    }
    
    cli::cli_warn(
      "{.arg {input}} is a sparse tibble, but {.fn {class(object)[1]}} with
      engine {.val {object$engine}} doesn't accept that. Converting to 
      non-sparse."
    )
    for (i in seq_along(ncol(x))) {
      # materialize with []
      x[[i]] <- x[[i]][]
    }
  }
  x
}
