#' Poisson regression models
#'
#' @description
#'
#' `poisson_reg()` defines a generalized linear model for count data that follow
#' a Poisson distribution. This function can fit regression models.
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
#' @param mixture A number between zero and one (inclusive) giving the
#'  proportion of L1 regularization (i.e. lasso) in the model.
#'
#'  * `mixture = 1` specifies a pure lasso model,
#'  * `mixture = 0`  specifies a ridge regression model, and
#'  * `0 < mixture < 1` specifies an elastic net model, interpolating lasso and ridge.
#'
#'  Available for `glmnet` and `spark` only.
#'
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("poisson_reg")}
#' @export
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
      user_specified_mode = !missing(mode),
      method = NULL,
      engine = engine,
      user_specified_engine = !missing(engine)
    )
  }

# ------------------------------------------------------------------------------

#' @method update poisson_reg
#' @rdname parsnip_update
#' @export
update.poisson_reg <-
  function(object,
           parameters = NULL,
           penalty = NULL, mixture = NULL,
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
      cls = "poisson_reg",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @export
translate.poisson_reg <- function(x, engine = x$engine, ...) {
  x <- translate.default(x, engine, ...)

  if (engine == "glmnet") {
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
