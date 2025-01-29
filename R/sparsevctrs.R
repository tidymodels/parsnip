#' Using sparse data with parsnip
#' 
#' You can figure out whether a given model engine supports sparse data by 
#' calling `get_encoding("name of model")` and looking at the `allow_sparse_x`
#' column.
#' 
#' Using sparse data for model fitting and prediction shouldn't require any 
#' additional configurations. Just pass in a sparse matrix such as dgCMatrix 
#' from the `Matrix` package or a sparse tibble from the sparsevctrs package 
#' to the data argument of [fit()], [fit_xy()], and [predict()].
#' 
#' Models that don't support sparse data will try to convert to non-sparse data 
#' with warnings. If conversion isn’t possible, an informative error will be 
#' thrown.
#' 
#' @name sparse_data
NULL

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

is_sparse_matrix <- function(x) {
  methods::is(x, "sparseMatrix")
}

materialize_sparse_tibble <- function(x, object, input) {
  if (sparsevctrs::has_sparse_elements(x) && (!allow_sparse(object))) {
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
