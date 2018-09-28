#' @export
nearest_neighbor <- function(mode = "unknown",
                             neighbors = NULL,
                             others = list(),
                             ...) {

  check_empty_ellipse(...)

  ## TODO: make a utility function here
  if (!(mode %in% nearest_neighbor_modes)) {
    stop("`mode` should be one of: ",
         paste0("'", nearest_neighbor_modes, "'", collapse = ", "),
         call. = FALSE)
  }

  if(is.numeric(neighbors) && !positive_int_scalar(neighbors)) {
    stop("`neighbors` must be a length 1 positive integer.", call. = FALSE)
  }

  args <- list(neighbors = neighbors)

  no_value <- !vapply(others, is.null, logical(1))
  others <- others[no_value]

  # is this the right place?
  # have to check if length = 1 b/c kernel is one of the parameters train.kknn()
  # optimizes over if multiple are given
  if(!is.null(others[["kernel"]]) && length(others[["kernel"]]) > 1) {
    stop("The length of `kernel` must be 1.", call. = FALSE)
  }

  # write a constructor function
  out <- list(args = args, others = others,
              mode = mode, method = NULL, engine = NULL)
  # TODO: make_classes has wrong order; go from specific to general
  class(out) <- make_classes("nearest_neighbor")
  out
}

#' @export
print.nearest_neighbor <- function(x, ...) {
  cat("K-Nearest Neighbor Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if(!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

#' @export
update.nearest_neighbor <- function(object,
                                    neighbors = NULL,
                                    others = list(),
                                    fresh = FALSE,
                                    ...) {

  check_empty_ellipse(...)

  if(is.numeric(neighbors) && !positive_int_scalar(neighbors)) {
    stop("`neighbors` must be a length 1 positive integer.", call. = FALSE)
  }

  args <- list(neighbors = neighbors)

  if (fresh) {
    object$args <- args
  } else {
    null_args <- map_lgl(args, null_value)
    if (any(null_args))
      args <- args[!null_args]
    if (length(args) > 0)
      object$args[names(args)] <- args
  }

  if (length(others) > 0) {
    if (fresh)
      object$others <- others
    else
      object$others[names(others)] <- others
  }

  object
}


positive_int_scalar <- function(x) {
  (length(x) == 1) && (x > 0) && (x %% 1 == 0)
}
