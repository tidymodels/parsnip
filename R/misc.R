
# make S3 with methods for vector, matrix, and recipe
guess_mode <- function(y) {
  if (inherits(y, c("character", "factor"))) {
    res <- "classification"
  } else if (inherits(y, "numeric")) {
    res <- "regression"
  } else if (inherits(y, "Surv")) {
    res <- "risk regression"
  } else res <- "unknown"
  res
}

# Q: make classes for mode too? 
make_classes <- function(prefix, mode) {
  cls <- c(paste(prefix, mode, sep = "."), prefix)
  c("model_spec", gsub(" ", "_", cls))
}


#' @importFrom rlang quos is_empty is_null
check_empty_ellipse <- function (...)  {
  terms <- quos(...)
  if (!is_empty(terms)) 
    stop("Please pass other arguments to the model function via `engine_args`", call. = FALSE)
  terms
}

all_modes <- c("classification", "regression")

model_printer <- function(x, ...) {
  non_null_args <- x$args[!vapply(x$args, null_value, lgl(1))]
  if (length(non_null_args) > 0) {
    cat("Main Arguments:\n")
    non_null_args <- lapply(non_null_args, as.character)
    non_null_args <- lapply(non_null_args, function(x)
      paste0("  ", x[-1], "\n"))
    anms <- names(non_null_args)
    non_null_args <- paste(anms, unlist(non_null_args), sep = ": ")
    cat(non_null_args, sep = "", "\n")
  } 
  if (length(x$others) > 0) {
    cat("Engine-Specific Arguments:\n")
    others <- lapply(x$others, function(x) paste(deparse(x), sep = "\n", collapse = "\n"))
    others <- lapply(others, function(x)
      paste0("  ", x, "\n"))
    onms <- names(others)
    others <- paste(onms, unlist(others), sep = ": ")
    cat(others, sep = "", "\n\n")
  }  
  if (!is.null(x$engine)) {
    cat("Computational engine:", x$engine, "\n\n")
    if (!is.null(x$method$fit)) {
      cat("Fit function:\n")
      print(x$method$fit)
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