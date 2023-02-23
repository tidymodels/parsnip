#' Proportional hazards regression
#'
#' @description
#' `proportional_hazards()` defines a model for the hazard function
#' as a multiplicative function of covariates times a baseline hazard. This
#' function can fit censored regression models.
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
      user_specified_mode = !missing(mode),
      method = NULL,
      engine = engine,
      user_specified_engine = !missing(engine)
    )
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

    args <- list(
      penalty = enquo(penalty),
      mixture = enquo(mixture)
    )

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "proportional_hazards",
      ...
    )
  }

#' @export
translate.proportional_hazards <- function(x, engine = x$engine, ...) {
  x <- translate.default(x, engine, ...)

  if (engine == "glmnet") {
    # See https://parsnip.tidymodels.org/reference/glmnet-details.html
    .check_glmnet_penalty_fit(x)
    if (any(names(x$eng_args) == "path_values")) {
      # Since we decouple the parsnip `penalty` argument from being the same
      # as the glmnet `lambda` value, `path_values` allows users to set the
      # path differently from the default that glmnet uses. See
      # https://github.com/tidymodels/parsnip/issues/431
      x$method$fit$args$lambda <- x$eng_args$path_values
      x$eng_args$path_values <- NULL
      x$method$fit$args$path_values <- NULL
    } else {
      # See discussion in https://github.com/tidymodels/parsnip/issues/195
      x$method$fit$args$lambda <- NULL
    }
    # Since the `fit` information is gone for the penalty, we need to have an
    # evaluated value for the parameter.
    x$args$penalty <- rlang::eval_tidy(x$args$penalty)
  }

  x
}
