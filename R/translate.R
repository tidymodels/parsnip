#' Resolve a Model Specification for a Computational Engine
#'
#' `translate` will translate a model specification into a code
#'  object that is specific to a particular engine (e.g. R package).
#'  It translates generic parameters to their counterparts.
#'
#' @param x A model specification.
#' @param ... Not currently used.
#' @details
#' `translate` produces a _template_ call that lacks the specific
#'  argument values (such as `data`, etc). These are filled in once
#'  `fit` is called with the specifics of the data for the model.
#'  The call may also include `varying` arguments if these are in
#'  the specification.
#'
#' It does contain the resolved argument names that are specific to
#'  the model fitting function/engine.
#'
#' This function can be useful when you need to understand how
#'  `parsnip` goes from a generic model specific to a model fitting
#'  function.
#' @examples
#' lm_spec <- linear_reg(penalty = 0.01)
#'
#' # `penalty` is tranlsated to `lambda`
#' translate(lm_spec, engine = "glmnet")
#'
#' # `penalty` not applicable for this model.
#' translate(lm_spec, engine = "lm")
#'
#' # `penalty` is tranlsated to `reg_param`
#' translate(lm_spec, engine = "spark")
#'
#' # with a placeholder for an unknown argument value:
#' translate(linear_reg(mixture = varying()), engine = "glmnet")
#'
#' @export

translate <- function (x, ...)
  UseMethod("translate")

#' @importFrom utils getFromNamespace
#' @importFrom purrr list_modify
#' @export
translate.default <- function(x, engine, ...) {
  check_empty_ellipse(...)

  x$engine <- engine
  x <- check_engine(x)

  if (is.null(x$method))
    x <- get_method(x, engine, ...)

  arg_key <- get_module(specific_model(x))

  # deharmonize primary arguments
  actual_args <- deharmonize(x$args, arg_key, x$engine)

  # check secondary arguments to see if they are in the final
  # expression unless there are dots, warn if protected args are
  # being altered
  eng_arg_key <- arg_key[[x$engine]]
  x$others <- check_others(x$others, x$method$fit, eng_arg_key)

  # keep only modified args
  modifed_args <- !vapply(actual_args, null_value, lgl(1))
  actual_args <- actual_args[modifed_args]

  # look for defaults if not modified in other
  if(length(x$method$fit$defaults) > 0) {
    in_other <- names(x$method$fit$defaults) %in% names(x$others)
    x$defaults <- x$method$fit$defaults[!in_other]
  }

  # combine primary, others, and defaults
  protected <- lapply(x$method$fit$protect, function(x) expr(missing_arg()))
  names(protected) <- x$method$fit$protect

  x$method$fit$args <- c(protected, actual_args, x$others, x$defaults)

  # put in correct order
  x
}

get_method <- function(x, engine, ...) {
  check_empty_ellipse(...)
  x$engine <- engine
  x <- check_engine(x)
  x$method <- get_model_info(x, x$engine)
  x
}


get_module <- function(nm) {
  arg_key <- try(
    getFromNamespace(
      paste0(nm, "_arg_key"),
      ns = "parsnip"
    ),
    silent = TRUE
  )
  if(inherits(arg_key, "try-error")) {
    arg_key <- try(
      get(paste0(nm, "_arg_key")),
      silent = TRUE
    )
  }
  if(inherits(arg_key, "try-error")) {
    stop(
      "Cannot find the model code: `",
      paste0(nm, "_arg_key"),
      "`", call. = FALSE
    )
  }
  arg_key
}


#' @export
print.model_spec <- function(x, ...) {
  cat("Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)
  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }
  invisible(x)
}

check_mode <- function(object, lvl) {
  if (object$mode == "unknown") {
    if (!is.null(lvl)) {
      object$mode <- "classification"
    } else {
      object$mode <- "regression"
    }
  }
  object
}
