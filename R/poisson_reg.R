#' Poisson regression models
#'
#' @description
#'
#' `poisson_reg()` defines a generalized linear model for count data that follow
#' a Poisson distribution.

#' There are different ways to fit this model. The method of estimation is
#' chosen by setting the model _engine_.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("poisson_reg")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams boost_tree
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param penalty A non-negative number representing the total
#'  amount of regularization (`glmnet` only).
#' @param mixture A number between zero and one (inclusive) that is the
#'  proportion of L1 regularization (i.e. lasso) in the model. When
#'  `mixture = 1`, it is a pure lasso model while `mixture = 0` indicates that
#'  ridge regression is being used. (`glmnet` and `spark` only).
#'
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("poisson_reg")}
#' @export
#' @importFrom purrr map_lgl
poisson_reg <-
  function(mode = "regression",
           penalty = NULL,
           mixture = NULL,
           engine = "glm") {

    args <- list(
      penalty = enquo(penalty),
      mixture = enquo(mixture)
    )

    new_model_spec(
      "poisson_reg",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.poisson_reg <- function(x, ...) {
  cat("Poisson Regression Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}


# ------------------------------------------------------------------------------

#' @param object A boosted tree model specification.
#' @param parameters A 1-row tibble or named list with _main_
#'  parameters to update. If the individual arguments are used,
#'  these will supersede the values in `parameters`. Also, using
#'  engine arguments in this object will result in an error.
#' @param ... Not used for `update()`.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @return An updated model specification.
#' @examples
#' model <- poisson_reg(penalty = 10, mixture = 0.1)
#' model
#' update(model, penalty = 1)
#' update(model, penalty = 1, fresh = TRUE)
#' @method update poisson_reg
#' @rdname poisson_reg
#' @export
update.poisson_reg <-
  function(object,
           parameters = NULL,
           penalty = NULL, mixture = NULL,
           fresh = FALSE, ...) {
    update_dot_check(...)

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
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
    }

    new_model_spec(
      "poisson_reg",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

#' @export
translate.poisson_reg <- function(x, engine = x$engine, ...) {
  x <- translate.default(x, engine, ...)

  if (engine == "glmnet") {
    # See discussion in https://github.com/tidymodels/parsnip/issues/195
    x$method$fit$args$lambda <- NULL
    # Since the `fit` information is gone for the penalty, we need to have an
    # evaluated value for the parameter.
    x$args$penalty <- rlang::eval_tidy(x$args$penalty)
  }

  x
}

# ------------------------------------------------------------------------------

check_args.poisson_reg <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (all(is.numeric(args$penalty)) && any(args$penalty < 0))
    rlang::abort("The amount of regularization should be >= 0.")
  if (is.numeric(args$mixture) && (args$mixture < 0 | args$mixture > 1))
    rlang::abort("The mixture proportion should be within [0,1].")
  if (is.numeric(args$mixture) && length(args$mixture) > 1)
    rlang::abort("Only one value of `mixture` is allowed.")

  invisible(object)
}

# ------------------------------------------------------------------------------

set_new_model("poisson_reg")
set_model_mode("poisson_reg", "regression")
