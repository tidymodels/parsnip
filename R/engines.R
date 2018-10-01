
get_model_info <-  function (x, engine)  {
  cls <- specific_model(x)
  nm <- paste(cls, engine, "data", sep = "_")
  res <- try(get(nm), silent = TRUE)
  if (inherits(res, "try-error"))
    stop("Can't find model object ", nm)
  res
}

specific_model <- function(x) {
  cls <- class(x)
  cls[cls != "model_spec"]
}


possible_engines <- function(object, ...) {
  cls <- specific_model(object)
  key_df <- get(paste(cls, "engines", sep = "_"))
  colnames(key_df[object$mode, , drop = FALSE])
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
  if (length(x$method$libs) > 0) {
    is_inst <- x$method$libs %in% lib_inst
    if (any(!is_inst)) {
      stop(
        "This engine requires some package installs: ",
        paste0("'", x$method$libs[!is_inst], "'", collapse = ", ")
      )
    }
  }
}
