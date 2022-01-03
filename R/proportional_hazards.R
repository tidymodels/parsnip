#' Proportional hazards regression
#'
#' @description
#' `proportional_hazards()` defines a technique that models the hazard function
#' as a multiplicative function of covariates times a baseline hazard. The
#' function can fit  censored regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("proportional_hazards")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams boost_tree
#' @inheritParams linear_reg
#' @param mode A single character string for the prediction outcome mode.
#'  The only possible value for this model is "censored regression".
#'
#' @template spec-details
#'
#' @template spec-survival
#'
#' @details
#' Proportional hazards models include the Cox model.
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("proportional_hazards")}
#'
#' @examples
#' show_engines("proportional_hazards")
#'
#' proportional_hazards(mode = "censored regression")
#' @keywords internal
#' @export
proportional_hazards <- function(
  mode = "censored regression",
  engine = "survival",
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
      engine = engine
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

#' @method update proportional_hazards
#' @rdname parsnip_update
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

#' @export
translate.proportional_hazards <- function(x, engine = x$engine, ...) {
  x <- translate.default(x, engine, ...)

  if (engine == "glmnet") {
    # See discussion in https://github.com/tidymodels/parsnip/issues/195
    x$method$fit$args$lambda <- NULL
    # Since the `fit` information is gone for the penalty, we need to have an
    # evaluated value for the parameter.
    x$args$penalty <- rlang::eval_tidy(x$args$penalty)
    .check_glmnet_penalty_fit(x)
  }

  x
}
