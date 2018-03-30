#' @import rlang


make_classes <- function(prefix) {
  c("model_spec", prefix)
}


check_empty_ellipse <- function (...)  {
  terms <- quos(...)
  if (!is_empty(terms)) 
    stop("Please pass other arguments to the model function via `others`", call. = FALSE)
  terms
}

all_modes <- c("classification", "regression")

print_list <- function(x, ...) 
  paste0("  ", names(x), " = ", format(x, ...), collaspe = "\n")



model_printer <- function(x, ...) {
  non_null_args <- x$args[!vapply(x$args, null_value, lgl(1))]
  if (length(non_null_args) > 0) {
    cat("Main Arguments:\n")
    cat(print_list(non_null_args), \n, sep = "")
  } 
  if (length(x$others) > 0) {
    cat("Engine-Specific Arguments:\n")
    cat(print_list(x$others), \n, sep = "")
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
  cat("\n")
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


