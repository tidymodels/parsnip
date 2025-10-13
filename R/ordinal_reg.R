#' Ordinal regression
#'
#' @description
#' `ordinal_reg()` defines a generalized linear model that predicts an ordinal
#' outcome. This function can fit classification models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("ordinal_reg")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @param mode A single character string for the prediction outcome mode. The
#'   only possible value for this model is "classification".
#' @param engine A single character string specifying what computational engine
#'  to use for fitting. Possible engines are listed below. The default for this
#'  model is `"polr"`.
#' @param ordinal_link The ordinal link function.
#' @param odds_link The odds or probability link function.
#' @param penalty A non-negative number representing the total
#'  amount of regularization (specific engines only).
#' @param mixture A number between zero and one (inclusive) denoting the
#'  proportion of L1 regularization (i.e. lasso) in the model.
#'
#'  * `mixture = 1` specifies a pure lasso model,
#'  * `mixture = 0`  specifies a ridge regression model, and
#'  * `0 < mixture < 1` specifies an elastic net model,
#'    interpolating lasso and ridge.
#'
#'  Available for specific engines only.
#'
#' @templateVar modeltype ordinal_reg
#'
#' @template spec-details
#'
#' @details Ordinal regression models include cumulative, sequential, and
#' adjacent structures.
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("ordinal_reg")}
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("ordinal_reg")
#'
#' ordinal_reg(mode = "classification")
#'
#' @keywords internal
#' @export
ordinal_reg <-
  function(
    mode = "classification",
    ordinal_link = NULL, odds_link = NULL, penalty = NULL, mixture = NULL,
    engine = "polr"
  ) {

    if (mode != "classification") {
      rlang::abort("`mode` should be 'classification'")
    }

    args <- list(
      ordinal_link = enquo(ordinal_link),
      odds_link = enquo(odds_link),
      penalty = enquo(penalty),
      mixture = enquo(mixture)
    )

    parsnip::new_model_spec(
      "ordinal_reg",
      args = args,
      eng_args = NULL,
      mode = mode,
      user_specified_mode = ! missing(mode),
      method = NULL,
      engine = engine,
      user_specified_engine = ! missing(engine)
    )
  }

# ------------------------------------------------------------------------------

#' @method update ordinal_reg
#' @rdname parsnip_update
#' @export
update.ordinal_reg <-
  function(
    object,
    parameters = NULL,
    ordinal_link = NULL, odds_link = NULL, penalty = NULL, mixture = NULL,
    fresh = FALSE, ...
  ) {

    args <- list(
      ordinal_link = enquo(ordinal_link),
      odds_link = enquo(odds_link),
      penalty = enquo(penalty),
      mixture = enquo(mixture)
    )

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "ordinal_reg",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @export
check_args.ordinal_reg <- function(object, call = rlang::caller_env()) {

  args <- lapply(object$args, rlang::eval_tidy)

  # copied from `check_args.linear_reg`
  check_number_decimal(args$mixture, min = 0, max = 1,
                       allow_null = TRUE, call = call, arg = "mixture")
  check_number_decimal(args$penalty, min = 0,
                       allow_null = TRUE, call = call, arg = "penalty")

  invisible(object)
}

# ------------------------------------------------------------------------------

#' @export
translate.ordinal_reg <- function(x, engine = x$engine, ...) {
  dots <- list(...)

  x <- translate.default(x, engine, ...)

  # adapted from `.check_glmnet_penalty_fit()`
  if (engine == "ordinalNet") {
    pen <- rlang::eval_tidy(x$args$penalty)
    if (length(pen) != 1) {
      cli::cli_abort(
        c(
          "x" = "For the ordinalNet engine, {.arg penalty} must be
          a single number (or a value of {.fn tune}).",
          "!" = "There are {length(pen)} value{?s} for {.arg penalty}.",
          "i" = "To try multiple values for total regularization,
          use the {.pkg tune} package.",
          "i" = "To predict multiple penalties, use {.fn multi_predict}."
        ),
        call = rlang::caller_env()
      )
    }

    # adapted from `set_glmnet_penalty_path()`
    if (any(names(x$eng_args) == "path_values")) {
      x$method$fit$args$lambdaVals <- x$eng_args$path_values
      x$eng_args$path_values <- NULL
      x$method$fit$args$path_values <- NULL
    } else {
    # } else if (! rlang::is_call(x$method$fit$args$lambdaVals)) {
      # REVIEW: `ordinalNet` models won't use values of `lambdaVals` at
      # predict-time outside the range used at fit-time. To enable a prediction
      # using a practical range of penalties _including the `penalty` value used
      # to fit_ (assuming a path wasn't specified), the code below passes values
      # to `ordinalNet()` arguments that ensure an extensive path that includes
      # the value passed to `penalty` (stored in `lambdaVals`). The alternative,
      # which i find equally reasonable, is to do nothing and disallow
      # predictions using any but the specified `penalty` parameter. Local
      # experiments suggest that, in contrast to `glmnet`, obtaining estimates
      # for the whole path can be much more expensive than for a single value.
      # The internal path calculation yields a maximum penalty that zeroes out
      # all penalized coefficients, so by including 0 we ensure that all values
      # can be interpolated.
      x$method$fit$args$nLambda <- 120L
      min_lambda <- if (rlang::is_call(x$method$fit$args$lambdaVals) ||
                        0 %in% x$method$fit$args$lambdaVals) {
        1e-08
      } else {
        min(x$method$fit$args$lambdaVals)
      }
      x$method$fit$args$lambdaMinRatio <- min_lambda
      x$method$fit$args$includeLambda0 <- TRUE
      x$method$fit$args$lambdaVals <- NULL
    }
    # Since the `fit` information is gone for the penalty, we need to have an
    # evaluated value for the parameter.
    x$args$penalty <- rlang::eval_tidy(x$args$penalty)

    # REVIEW: Below is an alternative solution to
    # `ordered::ordinal_net_score_wrapper`. It works in my examples. It seems
    # disfavored because it lives in {parsnip} rather than in {ordered}, but
    # there may be other considerations that make it preferable (or there may
    # be a better way than either.)

    # # translate odds link options
    # if (! is.null(x$method$fit$args$family)) {
    #   fam <- quo_get_expr(x$method$fit$args$family)
    #   fam <- match.arg(fam, c(
    #     "cumulative_link",
    #     "adjacent_categories",
    #     "continuation_ratio",
    #     "stopping_ratio"
    #   ))
    #   fam <- switch(
    #     fam,
    #     cumulative_link = "cumulative",
    #     adjacent_categories = "acat",
    #     continuation_ratio = "cratio",
    #     stopping_ratio = "sratio"
    #   )
    #   x$method$fit$args$family <- quo(!! fam)
    # }

  }

  x
}
