#' Linear regression
#'
#' @description
#'
#' `linear_reg()` defines a model that can predict numeric values from
#' predictors using a linear function. This function can fit regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("linear_reg")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param engine A single character string specifying what computational engine
#'  to use for fitting. Possible engines are listed below. The default for this
#'  model is `"lm"`.
#' @param penalty A non-negative number representing the total
#'  amount of regularization (specific engines only).
#' @param mixture A number between zero and one (inclusive) denoting the
#'  proportion of L1 regularization (i.e. lasso) in the model.
#'
#'  * `mixture = 1` specifies a pure lasso model,
#'  * `mixture = 0`  specifies a ridge regression model, and
#'  * `0 < mixture < 1` specifies an elastic net model, interpolating lasso and ridge.
#'
#'  Available for specific engines only.
#'
#' @templateVar modeltype linear_reg
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("linear_reg")}
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("linear_reg")
#'
#' linear_reg()
#' @export
linear_reg <-
  function(mode = "regression",
           engine = "lm",
           penalty = NULL,
           mixture = NULL) {

    args <- list(
      penalty = enquo(penalty),
      mixture = enquo(mixture)
    )

    new_model_spec(
      "linear_reg",
      args = args,
      eng_args = NULL,
      mode = mode,
      user_specified_mode = !missing(mode),
      method = NULL,
      engine = engine,
      user_specified_engine = !missing(engine)
    )
  }

#' @export
translate.linear_reg <- function(x, engine = x$engine, ...) {
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

#' @method update linear_reg
#' @rdname parsnip_update
#' @export
update.linear_reg <-
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
      cls = "linear_reg",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @export
check_args.linear_reg <- function(object, call = rlang::caller_env()) {

  args <- lapply(object$args, rlang::eval_tidy)

  check_number_decimal(args$mixture, min = 0, max = 1, allow_null = TRUE, call = call, arg = "mixture")
  check_number_decimal(args$penalty, min = 0, allow_null = TRUE, call = call, arg = "penalty")

  # ------------------------------------------------------------------------------
  # We want to avoid folks passing in a poisson family instead of using
  # poisson_reg(). It's hard to detect this.

  is_fam <- names(object$eng_args) == "family"
  if (any(is_fam)) {
    eng_args <- rlang::eval_tidy(object$eng_args[[which(is_fam)]])
    if (is.function(eng_args)) {
      eng_args <- try(eng_args(), silent = TRUE)
    }
    if (inherits(eng_args, "family")) {
      eng_args <- eng_args$family
    }
    if (eng_args == "poisson") {
      cli::cli_abort(
        "A Poisson family was requested for {.fn linear_reg}. Please use
        {.fn poisson_reg} and the engines in the {.pkg poissonreg} package.",
        call = rlang::call2("linear_reg"))
    }
  }

  invisible(object)
}
