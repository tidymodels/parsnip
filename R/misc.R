#' @import rlang


make_classes <- function(prefix) {
  c(prefix, "model_spec")
}


check_empty_ellipse <- function (...)  {
  terms <- quos(...)
  if (!is_empty(terms)) 
    stop("Please pass other arguments to the model function via `others`", call. = FALSE)
  terms
}

all_modes <- c("classification", "regression")


deparserizer <- function(x, limit = options()$width - 10) {
  x <- deparse(x, width.cutoff = limit)
  x <- gsub("^    ", "", x)
  x <- paste0(x, collapse = "")
  if (nchar(x) > limit)
    x <- paste0(substring(x, first = 1, last = limit - 7), "<snip>")
  x
}

print_arg_list <- function(x, ...) {
  others <- c("name", "call", "expression")
  atomic <- vapply(x, is.atomic, logical(1))
  x2 <- x
  x2[atomic] <- lapply(x2[atomic], format, ...)
  x2[!atomic] <-  lapply(x2[!atomic], deparserizer, ...)
  paste0("  ", names(x2), " = ", x2, collaspe = "\n")
}

model_printer <- function(x, ...) {
  non_null_args <- x$args[!vapply(x$args, null_value, lgl(1))]
  if (length(non_null_args) > 0) {
    cat("Main Arguments:\n")
    cat(print_arg_list(non_null_args), "\n", sep = "")
  } 
  if (length(x$others) > 0) {
    cat("Engine-Specific Arguments:\n")
    cat(print_arg_list(x$others), "\n", sep = "")
  }  
  if (!is.null(x$engine)) {
    cat("Computational engine:", x$engine, "\n\n")
    if (!is.null(x$method$fit_call)) {
      cat("Fit function:\n")
      print(x$method$fit_call)
      if (length(x$method$library) > 0) {
        if (length(x$method$library) > 1)
          cat("\nRequired packages:\n")
        else
          cat("\nRequired package: ")
        cat(paste0(x$method$library, collapse = ", "), "\n")
      }
    }
  }
}

load_libs <- function(x, quiet) {
  if (quiet) {
    for (pkg in x$method$library)
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  } else {
    for (pkg in x$method$library)
      library(pkg, character.only = TRUE)
  }
  invisible(x)
}

is_missing_arg <- function(x)
  identical(x, quote(missing_arg()))


