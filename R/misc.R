
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


get_model_objects <- function(x, engine) {
  if(x$mode == "unknown")
    stop("Please specify a mode for the model (e.g. regression, classification, etc.) ", 
         "so that the model code can be finalized", call. = FALSE)
  nm <- paste("get", engine, x$mode, sep = "_")
  res <- try(get(nm), silent = TRUE)
  if(inherits(res, "try-error"))
    stop("Can't find model object ", nm)
  res
}

#' @importFrom rlang quos is_empty is_null
check_empty_ellipse <- function (...)  {
  terms <- quos(...)
  if (!is_empty(terms)) 
    stop("Please pass other arguments to the model function via `engine_args`", call. = FALSE)
  terms
}

