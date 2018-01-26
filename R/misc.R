
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

