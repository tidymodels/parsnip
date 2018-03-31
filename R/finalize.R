#' Resolve a Model Specification for a Computational Engine
#' 
#' `finalize` will translate a model specification into a code
#'  object that is specific to a particular engine (e.g. R package).
#'  It translates generic parameters to their counterparts.
#' 
#' @param x A model specification.
#' @param ... Not currently used. 
#' @export
#' 
# TODO: maybe change name to `translate` since it won't be finalized until there
# is data?

finalize <- function (x, ...)
  UseMethod("finalize")

#' @importFrom utils getFromNamespace
#' @export
finalize.default <- function(x, engine, ...) {
  check_empty_ellipse(...)
  x$engine <- engine
  x <- check_engine(x)
  x$method <- get_model_objects(x, x$engine)
  
  arg_key <- getFromNamespace(
    paste0(specifc_model(x), "_arg_key"),
    ns = "parsnip"
    )
  
  # deharmonize primary arguments
  real_args <- deharmonize(x$args, arg_key, x$engine)
  
  # check secondary arguments to see if they are in the final 
  # expression unless there are dots
  x$others <- check_others(x$others, x$method)
  
  # sub in args
  modifed_args <-
    names(real_args)[!vapply(real_args, null_value, lgl(1))]
  for(i in modifed_args) {
    x$method$fit[[i]] <- rlang::get_expr(real_args[[i]])
  }
  if(length(x$others) > 0) {
    for(i in names(x$others)) {
      if (all(i != x$method$protect))
        x$method$fit[[i]] <- rlang::get_expr(x$others[[i]])
      else
        warning("Argument ", i, " cannot be modified", call. = FALSE)
    } 
  } 
  
  # trim args that are defaults or null
  x$method$fit <-
    prune_arg_list(x$method$fit, x$method$protect, c(modifed_args, names(x$others)))
  
  # create call
  # TODO determine how to construct call with the namespace operator ("stats::glm")
  x$method$fit_call <- call(x$method$fit_name)
  for(i in names(x$method$fit)) {
    if(!is.null(x$method$fit[[i]])) 
      x$method$fit_call[[i]] <- x$method$fit[[i]]
  }
  
  x
}
