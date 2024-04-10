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
#' @inheritParams nearest_neighbor
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
#' @templateVar modeltype poisson_reg
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
    # See https://parsnip.tidymodels.org/reference/glmnet-details.html
    .check_glmnet_penalty_fit(x)
    x <- set_glmnet_penalty_path(x)
    # Since the `fit` information is gone for the penalty, we need to have an
    # evaluated value for the parameter.
    x$args$penalty <- rlang::eval_tidy(x$args$penalty)
  }

  x
}

# ------------------------------------------------------------------------------

#' @export
check_args.poisson_reg <- function(object, call = rlang::caller_env()) {

  args <- lapply(object$args, rlang::eval_tidy)

  check_number_decimal(args$mixture, min = 0, max = 1, allow_null = TRUE, call = call, arg = "mixture")
  check_number_decimal(args$penalty, min = 0, allow_null = TRUE, call = call, arg = "penalty")

  invisible(object)
}

# ------------------------------------------------------------------------------

set_new_model("poisson_reg")
set_model_mode("poisson_reg", "regression")
