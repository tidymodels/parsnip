
get_model_objects <- function(x, engine) {
  if(x$mode == "unknown")
    stop("Please specify a mode for the model (e.g. regression, classification, etc.) ", 
         "so that the model code can be finalized", call. = FALSE)
  cls <- class(x)
  cls <- cls[cls != "model_spec"]
  # This is a short term hack to get most general class
  # Q: do we need mode-specific classes?
  cls <- cls[which.min(nchar(cls))]
  nm <- paste(cls, engine, x$mode, sep = "_")
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

check_engine <- function(object) {
  avail_eng <- possible_engines(object)
  if (is.null(object$engine)) {
    object$engine <- avail_eng[1]
    warning("`engine` was NULL and updated to be '",
            object$engine, "'", call. = FALSE)
  }
  if (!(object$engine %in% avail_eng)) {
    stop(
      "engine '",object$engine,
      "' is not availble. Please use ",  "one of: ",
      paste0("'", avail_eng, "'", collapse = ", "),
      call. = FALSE
    )
  }
  object
}

#' @importFrom utils installed.packages
check_installs <- function(x) {
  lib_inst <- rownames(installed.packages())
  if (length(x$method$library) > 0) {
    is_inst <- x$method$library %in% lib_inst
    if (any(!is_inst)) {
      stop(
        "This engine requires some package installs: ",
        paste0("'", x$method$library[!is_inst], "'", collapse = ", ")
      )
    }
  }
}