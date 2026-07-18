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
#' @param parallel_reg A specification for the parallel regression assumption.
#'  Possible values are:
#'
#'  * `TRUE`: all terms parallel. This is the default.
#'  * `FALSE`: all terms non-parallel.
#'  * A formula with a logical LHS: `TRUE ~ x1 + x2` names the parallel
#'    terms; `FALSE ~ x1 + x2` names the non-parallel terms.
#'  * A list of at most two of the above (at most one per logical value):
#'    specifies partial proportional odds, where some terms are parallel and
#'    others are non-parallel, possibly with overlap.
#'
#'  Available for specific engines only, and different engines accept different
#'  specifications.
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
      rlang::abort("`mode` should be 'classification'")
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

  if (! is.null(args$parallel_reg)) {
    validate_parallel_reg(args$parallel_reg, call = call)
  }

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

  invisible(object)
}

# ------------------------------------------------------------------------------

#' @export
translate.ordinal_reg <- function(x, engine = x$engine, ...) {
  dots <- list(...)

  x <- translate.default(x, engine, ...)

  # Reject `parallel_reg` for engines that don't support assumption violations
  if (! engine %in% c("clm", "vglm", "ordinalNet", "brms")) {
    pr <- rlang::eval_tidy(x$args$parallel_reg)
    if (! is.null(pr) && ! (isTRUE(pr))) {
      cli::cli_abort(
        c(
          "The {.val {engine}} engine does not support relaxing the
          proportional odds assumption.",
          "i" = "Use engine {.val clm} or {.val vglm} for non-proportional
          odds models."
        ),
        call = rlang::caller_env()
      )
    }
  }

  # REVIEW: What's the preferred way to flag when a legitimate model parameter
  # is passed a value that the engine doesn't accept?
  if (engine == "polr") {
    oddslink <- rlang::eval_tidy(x$args$odds_link)
    if (!is.null(oddslink) && oddslink != "cumulative_link") {
      cli::cli_warn(
        c(
          "!" = "The polr engine uses the cumulative link odds link;
          {.arg odds_link} will be ignored."
        ),
        call = rlang::caller_env()
      )
    }
  }

  if (engine == "clm") {
    link_arg <- x$method$fit$args$link
    if (rlang::is_quosure(link_arg)) {
      link_val <- rlang::eval_tidy(link_arg)
    } else {
      link_val <- link_arg
    }
    if (
      is.character(link_val) && length(link_val) == 1L && link_val == "logistic"
    ) {
      x$method$fit$args$link <- rlang::new_quosure("logit", rlang::empty_env())
    }

    thresh_arg <- x$method$fit$args$threshold
    if (rlang::is_quosure(thresh_arg)) {
      thresh_val <- rlang::eval_tidy(thresh_arg)
    } else {
      thresh_val <- thresh_arg
    }
    if (is.character(thresh_val) && length(thresh_val) == 1L) {
      thresh_val <- switch(
        thresh_val,
        symmetric_median = "symmetric",
        symmetric_zero = "symmetric2",
        thresh_val
      )
      x$method$fit$args$threshold <- thresh_val
    }
  }

  # adapted from `.check_glmnet_penalty_fit()`
  if (engine == "ordinalNet") {
    pen <- rlang::eval_tidy(x$args$penalty)
    if (length(pen) != 1L) {
      msg <- c(
        "x" = "The ordinalNet engine ignores {.arg penalty} in favor of a
          path that enables prediction at interpolated penalty values.",
        "!" = "{.arg penalty} was passed {length(pen)} value{?s}.",
        "i" = "Use `path_values` to override the default path."
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
      cli::cli_warn(msg, call = rlang::caller_env())
    }

    # adapted from `set_glmnet_penalty_path()`
    if (any(names(x$eng_args) == "path_values")) {
      x$method$fit$args$lambdaVals <- x$eng_args$path_values
      x$eng_args$path_values <- NULL
      x$method$fit$args$path_values <- NULL
    } else {
      # } else if (! rlang::is_call(x$method$fit$args$lambdaVals)) {
      # NOTES: `ordinalNet` models won't use values of `lambdaVals` at
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
      min_lambda <-
        if (
          rlang::is_call(x$method$fit$args$lambdaVals) ||
            is.null(x$method$fit$args$lambdaVals) ||
            0 %in% x$method$fit$args$lambdaVals
        ) {
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
  }

  if (engine == "glmnetcr") {
    pen <- rlang::eval_tidy(x$args$penalty)
    if (length(pen) != 1L) {
      msg <- c(
        "x" = "The glmnetcr engine ignores {.arg penalty} in favor of a
          path that enables prediction at interpolated penalty values.",
        "!" = "{.arg penalty} was passed {length(pen)} value{?s}.",
        "i" = "Use `path_values` to override the default path."
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
      cli::cli_warn(msg, call = rlang::caller_env())
    }

    if (any(names(x$eng_args) == "path_values")) {
      x$method$fit$args$lambda <- x$eng_args$path_values
      x$eng_args$path_values <- NULL
      x$method$fit$args$path_values <- NULL
    } else {
      x$method$fit$args$nlambda <- 120L
      min_pen <-
        if (
          rlang::is_call(x$method$fit$args$lambda) ||
            is.null(x$method$fit$args$lambda) ||
            0 %in% x$method$fit$args$lambda
        ) {
          1e-08
        } else {
          min(x$method$fit$args$lambda)
        }
      x$method$fit$args$lambda.min.ratio <- min_pen
      x$method$fit$args$lambda <- NULL
    }
    # Since the `fit` information is gone for the penalty, we need to have an
    # evaluated value for the parameter.
    x$args$penalty <- rlang::eval_tidy(x$args$penalty)
  }

  x
}

# ------------------------------------------------------------------------------

validate_parallel_reg <- function(x, call = rlang::caller_env()) {
  if (is.logical(x) && length(x) == 1L) {
    return(invisible())
  }

  if (inherits(x, "formula") && length(x) == 3L) {
    lhs <- x[[2L]]
    if (!is.logical(lhs) || length(lhs) != 1L) {
      cli::cli_abort(
        "The LHS of {.arg parallel_reg} formula must be TRUE or FALSE.",
        call = call
      )
    }
    return(invisible())
  }

  # allow to pass `c(FALSE, TRUE)`
  if (is.logical(x) && is.vector(x)) {
    x <- as.list(x)
  }
  if (is.list(x)) {
    if (length(x) > 2L) {
      cli::cli_abort(
        "{.arg parallel_reg} list can have at most 2 elements.",
        call = call
      )
    }
    lgl_vals <- character(0L)
    for (el in x) {
      if (is.logical(el) && length(el) == 1L) {
        lgl_vals <- c(lgl_vals, as.character(el))
      } else if (inherits(el, "formula") && length(el) == 3L) {
        lhs <- el[[2L]]
        if (!is.logical(lhs) || length(lhs) != 1L) {
          cli::cli_abort(
            "The LHS of each {.arg parallel_reg} formula must be TRUE or
            FALSE.",
            call = call
          )
        }
        lgl_vals <- c(lgl_vals, as.character(lhs))
      } else {
        cli::cli_abort(
          "Each element of {.arg parallel_reg} list must be a single logical
          value or a formula with a logical LHS.",
          call = call
        )
      }
    }
    if (any(duplicated(lgl_vals))) {
      cli::cli_abort(
        "{.arg parallel_reg} list cannot have duplicate logical values
        (e.g. two entries with TRUE).",
        call = call
      )
    }
    return(invisible())
  }

  cli::cli_abort(
    "{.arg parallel_reg} must be a single logical value, a formula with a
    logical LHS, or a list of at most two such elements.",
    call = call
  )
}

