#' General Interface for Proportional Hazards Models
#'
#' `proportional_hazards()` is a way to generate a _specification_ of a model
#' before fitting and allows the model to be created using different packages
#' in R. The main arguments for the model are:
#' \itemize{
#'   \item \code{penalty}: The total amount of regularization
#'  in the model. Note that this must be zero for some engines.
#'   \item \code{mixture}: The mixture amounts of different types of
#'   regularization (see below). Note that this will be ignored for some engines.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and arguments can be
#'  set using `set_engine()`. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions. If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", or "censored regression".
#' @inheritParams linear_reg
#'
#' @details
#' Proportional hazards models include the Cox model.
#' For `proportional_hazards()`, the mode will always be "censored regression".
#'
#' @examples
#' show_engines("proportional_hazards")
#'
#' @export
proportional_hazards <- function(mode = "censored regression",
                    penalty = NULL,
                    mixture = NULL) {

    args <- list(
      penalty = enquo(penalty),
      mixture = enquo(mixture)
    )

    new_model_spec(
      "proportional_hazards",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.proportional_hazards <- function(x, ...) {
  cat("Proportional Hazards Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @param object A proportional hazards model specification.
#' @param ... Not used for `update()`.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @examples
#' model <- proportional_hazards(penalty = 10, mixture = 0.1)
#' model
#' update(model, penalty = 1)
#' update(model, penalty = 1, fresh = TRUE)
#' @method update proportional_hazards
#' @rdname proportional_hazards
#' @export
update.proportional_hazards <- function(object,
                                        parameters = NULL,
                                        penalty = NULL,
                                        mixture = NULL,
                                        fresh = FALSE, ...) {

    eng_args <- update_engine_parameters(object$eng_args, ...)

    if (!is.null(parameters)) {
      parameters <- check_final_param(parameters)
    }
    args <- list(
      penalty = enquo(penalty),
      mixture = enquo(mixture)
    )

    args <- update_main_parameters(args, parameters)

    if (fresh) {
      object$args <- args
      object$eng_args <- eng_args
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
      if (length(eng_args) > 0)
        object$eng_args[names(eng_args)] <- eng_args
    }

    new_model_spec(
      "proportional_hazards",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }
