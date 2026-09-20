#' Ordinal regression
#'
#' @description
#' `ordinal_reg()` defines a generalized linear model that predicts an ordinal
#' outcome. This function can fit classification models.
#'
#' `Rd parsnip:::make_engine_list("ordinal_reg")`
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
#' @param threshold_structure The threshold structure for the cutpoints
#'  (specific engines only).
#' @param parallel_reg Logical; whether predictor effects are shared across
#'   thresholds (`TRUE`) or category-specific effects (`FALSE`). The default,
#'   `NULL`, uses the engine default. Available for specific engines only.
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
#' @seealso `Rd parsnip:::make_seealso_list("ordinal_reg")`
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
    ordinal_link = NULL,
    odds_link = NULL,
    threshold_structure = NULL,
    parallel_reg = NULL,
    penalty = NULL,
    mixture = NULL,
    engine = "polr"
  ) {
    if (mode != "classification") {
      cli::cli_abort("{.arg mode} should be {.val classification}.")
    }

    args <- list(
      ordinal_link = enquo(ordinal_link),
      odds_link = enquo(odds_link),
      threshold_structure = enquo(threshold_structure),
      parallel_reg = enquo(parallel_reg),
      penalty = enquo(penalty),
      mixture = enquo(mixture)
    )

    parsnip::new_model_spec(
      "ordinal_reg",
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

#' @method update ordinal_reg
#' @rdname parsnip_update
#' @export
update.ordinal_reg <-
  function(
    object,
    parameters = NULL,
    ordinal_link = NULL,
    odds_link = NULL,
    threshold_structure = NULL,
    parallel_reg = NULL,
    penalty = NULL,
    mixture = NULL,
    fresh = FALSE,
    ...
  ) {
    args <- list(
      ordinal_link = enquo(ordinal_link),
      odds_link = enquo(odds_link),
      threshold_structure = enquo(threshold_structure),
      parallel_reg = enquo(parallel_reg),
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

  check_bool(
    args$parallel_reg,
    allow_null = TRUE,
    call = call,
    arg = "parallel_reg"
  )

  # copied from `check_args.linear_reg`
  check_number_decimal(
    args$mixture,
    min = 0,
    max = 1,
    allow_null = TRUE,
    call = call,
    arg = "mixture"
  )
  check_number_decimal(
    args$penalty,
    min = 0,
    allow_null = TRUE,
    call = call,
    arg = "penalty"
  )

  # engine compatibility checks
  check_ordinal_reg_odds_link(object, object$engine, call = call)
  check_ordinal_reg_parallel(object, object$engine, call = call)
  if (object$engine == "clm") {
    check_clm_nominal(object, call = call)
  }

  invisible(object)
}


check_ordinal_reg_parallel <- function(x, engine, call = rlang::caller_env()) {
  # reject `parallel_reg` for engines that don't support assumption violations
  if (!engine %in% c("clm", "vglm", "ordinalNet")) {
    pr <- rlang::eval_tidy(x$args$parallel_reg)
    if (!is.null(pr) && !(isTRUE(pr))) {
      cli::cli_abort(
        c(
          "The {.val {engine}} engine does not support relaxing the
          parallel regression assumption.",
          "i" = "Use the {.val clm}, {.val vglm}, or {.val ordinalNet} engine
          for non-parallel models."
        ),
        call = call
      )
    }
  }

  invisible(NULL)
}

check_clm_nominal <- function(x, call = rlang::caller_env()) {
  # `nominal` is `clm()`'s own way of relaxing the assumption for a subset of
  # predictors, so it can only be used when `parallel_reg` is left unset
  if (is.null(x$eng_args$nominal)) {
    return(invisible(NULL))
  }

  parallel_reg <- rlang::eval_tidy(x$args$parallel_reg)
  if (!is.null(parallel_reg)) {
    cli::cli_abort(
      c(
        "{.arg parallel_reg} and the {.arg nominal} engine argument cannot both
         be used.",
        "i" = "{.arg nominal} relaxes the parallel regression assumption for
         the predictors it names; omit {.arg parallel_reg} to use it."
      ),
      call = call
    )
  }

  invisible(NULL)
}

check_ordinal_reg_odds_link <- function(x, engine, call = rlang::caller_env()) {
  if (engine %in% c("polr", "clm", "lrm", "orm")) {
    oddslink <- rlang::eval_tidy(x$args$odds_link)
    if (!is.null(oddslink) && oddslink != "cumulative_link") {
      cli::cli_abort(
        c(
          "The {.val {engine}} engine supports only the cumulative odds link.",
          "i" = "Use the {.val vglm} or {.val ordinalNet} engine
          for alternative odds links."
        ),
        call = call
      )
    }
  }

  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @export
translate.ordinal_reg <- function(
  x,
  engine = x$engine,
  ...,
  call = rlang::caller_env()
) {
  x <- translate.default(x, engine, ...)

  # penalty path assembly
  if (engine == "ordinalNet") {
    x <- translate_ordinal_reg_ordinalNet(x, call = call)
  }
  if (engine == "glmnetcr") {
    x <- translate_ordinal_reg_glmnetcr(x, call = call)
  }

  x
}

translate_ordinal_reg_ordinalNet <- function(x, call = rlang::caller_env()) {
  check_ordinal_reg_penalty(x$args$penalty, "ordinalNet", call = call)

  # adapted from `set_glmnet_penalty_path()`
  if (any(names(x$eng_args) == "path_values")) {
    x$method$fit$args$lambdaVals <- x$eng_args$path_values
    x$eng_args$path_values <- NULL
    x$method$fit$args$path_values <- NULL
  } else {
    # `ordinalNet` cannot predict outside its fitted penalty range. Generate a
    # path that includes the requested penalty and zero, noting that fitting the
    # full path can be substantially more expensive than fitting one value.
    x$method$fit$args$nLambda <- 120L
    if (
      rlang::is_call(x$method$fit$args$lambdaVals) ||
        is.null(x$method$fit$args$lambdaVals) ||
        0 %in% x$method$fit$args$lambdaVals
    ) {
      x$method$fit$args$lambdaMinRatio <- 1e-08
    } else {
      x$method$fit$args$lambdaMinRatio <-
        min(x$method$fit$args$lambdaVals)
    }
    x$method$fit$args$includeLambda0 <- TRUE
    x$method$fit$args$lambdaVals <- NULL
  }
  # Since the `fit` information is gone for the penalty, we need to have an
  # evaluated value for the parameter.
  x$args$penalty <- rlang::eval_tidy(x$args$penalty)

  x
}

translate_ordinal_reg_glmnetcr <- function(x, call = rlang::caller_env()) {
  check_ordinal_reg_penalty(x$args$penalty, "glmnetcr", call = call)

  if (any(names(x$eng_args) == "path_values")) {
    x$method$fit$args$lambda <- x$eng_args$path_values
    x$eng_args$path_values <- NULL
    x$method$fit$args$path_values <- NULL
  } else {
    x$method$fit$args$nlambda <- 120L
    if (
      rlang::is_call(x$method$fit$args$lambda) ||
        is.null(x$method$fit$args$lambda) ||
        0 %in% x$method$fit$args$lambda
    ) {
      x$method$fit$args$lambda.min.ratio <- 1e-08
    } else {
      x$method$fit$args$lambda.min.ratio <-
        min(x$method$fit$args$lambda)
    }
    x$method$fit$args$lambda <- NULL
  }
  # Since the `fit` information is gone for the penalty, we need to have an
  # evaluated value for the parameter.
  x$args$penalty <- rlang::eval_tidy(x$args$penalty)

  x
}

# adapted from `.check_glmnet_penalty_fit()`
check_ordinal_reg_penalty <- function(
  penalty,
  engine,
  call = rlang::caller_env()
) {
  pen <- rlang::eval_tidy(penalty)
  if (length(pen) != 1L) {
    msg <- c(
      "x" = "The {.val {engine}} engine ignores {.arg penalty} in favor of a
        path that enables prediction at interpolated penalty values.",
      "!" = "{.arg penalty} was passed {length(pen)} value{?s}.",
      "i" = "Use {.arg path_values} to override the default path."
    )
    if (length(pen) > 1L) {
      msg <- c(
        msg,
        c(
          "i" = "To specify multiple values for total regularization,
            use the {.pkg tune} package."
        )
      )
    }
    cli::cli_warn(msg, call = call)
  }

  invisible(NULL)
}
