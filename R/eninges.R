
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

possible_engines <- function(object, ...) {
  cls <- class(object)
  cls <- cls[cls != "model_spec"]
  # This is a short term hack to get most general class
  # Q: do we need mode-specific classes?
  cls <- cls[which.min(nchar(cls))]
  
  key_df <- get(paste(cls, "engines", sep = "_"))
  colnames(key_df[object$mode, ])
}

