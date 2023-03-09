#' Prepend a new class
#'
#' This adds an extra class to a base class of "model_spec".
#'
#' @param prefix A character string for a class.
#' @return A character vector.
#' @keywords internal
#' @export
make_classes <- function(prefix) {
  c(prefix, "model_spec")
}

#' Check to ensure that ellipses are empty
#' @param ... Extra arguments.
#' @return If an error is not thrown (from non-empty ellipses), a NULL list.
#' @keywords internal
#' @export
check_empty_ellipse <- function(...) {
  terms <- quos(...)
  if (!is_empty(terms)) {
    rlang::abort("Please pass other arguments to the model function via `set_engine()`.")
  }
  terms
}

is_missing_arg <- function(x) {
  identical(x, quote(missing_arg()))
}

# return a condition for use in `dplyr::filter()` on model info.
# if the user specified an engine and the model object reflects that in
# the `user_specified_engine` slot, filter the model info down to
# those that the user specified. if not, don't filter the model info at all.
#
# note that, model objects generated pre parsnip 1.0.2, or from extensions
# that don't implement the `user_specified_engine` slot, will not trigger
# these checks.
engine_filter_condition <- function(engine, user_specified_engine) {
  # use !isTRUE so that result is TRUE if is.null(user_specified_engine)
  if (!isTRUE(user_specified_engine) || is.null(engine))  {
    return(TRUE)
  }

  rlang::quo(engine == !!engine)
}

# analogous helper for modes to `engine_filter_condition()`
mode_filter_condition <- function(mode, user_specified_mode) {
  # use !isTRUE so that result is TRUE if is.null(user_specified_mode)
  if (!isTRUE(user_specified_mode) || is.null(mode))  {
    return(TRUE)
  }

  rlang::quo(mode == !!mode)
}

#' Model Specification Checking:
#'
#' The helpers `spec_is_possible()`, `spec_is_loaded()`, and
#' `prompt_missing_implementation()` provide tooling for checking
#' model specifications. In addition to the `spec`, `engine`, and `mode`
#' arguments, the functions take arguments `user_specified_engine` and
#' `user_specified_mode`, denoting whether the user themselves has
#' specified the engine or mode, respectively.
#'
#' `spec_is_possible()` checks against the union of
#'
#' * the current parsnip model environment and
#' * the `model_info_table` of "pre-registered" model specifications
#'
#' to determine whether a model is well-specified. See
#' `parsnip:::model_info_table` for this table.
#'
#' `spec_is_loaded()` checks only against the current parsnip model environment.
#'
#' `spec_is_possible()` is executed automatically on `new_model_spec()`,
#' `set_mode()`, and `set_engine()`, and `spec_is_loaded()` is executed
#' automatically in `print.model_spec()`, among other places. `spec_is_possible()`
#' should be used when a model specification is still "in progress" of being
#' specified, while `spec_is_loaded` should only be called when parsnip or an
#' extension receives some indication that the user is "done" specifying a model
#' specification: at print, fit, addition to a workflow, or `extract_*()`, for
#' example.
#'
#' When `spec_is_loaded()` is `FALSE`, the `prompt_missing_implementation()`
#' helper will construct an informative message to prompt users to load or
#' install needed packages. It's `prompt` argument refers to the prompting
#' function to use, usually [cli::cli_inform] or [cli::cli_abort], and the
#' ellipses are passed to that function.
#' @export
#' @keywords internal
#' @rdname extension-check-helpers
spec_is_possible <- function(spec,
                             engine = spec$engine,
                             user_specified_engine = spec$user_specified_engine,
                             mode = spec$mode,
                             user_specified_mode = spec$user_specified_mode) {
  cls <- class(spec)[[1]]

  all_model_info <-
    dplyr::full_join(
      model_info_table,
      rlang::env_get(get_model_env(), cls) %>% dplyr::mutate(model = cls),
      by = c("model", "engine", "mode")
    )

  engine_condition <- engine_filter_condition(engine, user_specified_engine)
  mode_condition <- mode_filter_condition(mode, user_specified_mode)

  possibilities <-
    all_model_info %>%
    dplyr::filter(
      model == cls,
      !!engine_condition,
      !!mode_condition
    )

  return(nrow(possibilities) > 0)
}

# see ?add_on_exports for more information on usage
#' @export
#' @keywords internal
#' @rdname extension-check-helpers
spec_is_loaded <- function(spec,
                           engine = spec$engine,
                           user_specified_engine = spec$user_specified_engine,
                           mode = spec$mode,
                           user_specified_mode = spec$user_specified_mode) {
  cls <- class(spec)[[1]]

  engine_condition <- engine_filter_condition(engine, user_specified_engine)
  mode_condition <- mode_filter_condition(mode, user_specified_mode)

  avail <- get_from_env(cls)

  if (is.null(avail)) {
    return(FALSE)
  }

  avail <- avail %>%
    dplyr::filter(!!mode_condition, !!engine_condition)

  if (nrow(avail) > 0) {
    return(TRUE)
  }

  FALSE
}

is_printable_spec <- function(x) {
  !is.null(x$method$fit$args) &&
    spec_is_loaded(x)
}

# construct a message informing the user that there are no
# implementations for the current model spec / mode / engine.
#
# if there's a "pre-registered" extension supporting that setup,
# nudge the user to install/load it.
#
# see ?add_on_exports for more information on usage
#' @export
#' @keywords internal
#' @rdname extension-check-helpers
prompt_missing_implementation <- function(spec,
                                          engine = spec$engine,
                                          user_specified_engine = spec$user_specified_engine,
                                          mode = spec$mode,
                                          user_specified_mode = spec$user_specified_mode,
                                          prompt, ...) {
  cls <- class(spec)[[1]]

  engine_condition <- engine_filter_condition(engine, user_specified_engine)
  mode_condition <- mode_filter_condition(mode, user_specified_mode)

  avail <- get_from_env(cls)

  if (!is.null(avail)) {
    avail <-
      avail %>%
      dplyr::filter(!!mode_condition, !!engine_condition)
  }

  all <-
    model_info_table %>%
    dplyr::filter(model == cls, !!mode_condition, !!engine_condition, !is.na(pkg)) %>%
    dplyr::select(-model)

  if (!isTRUE(user_specified_mode)) {mode <- ""}

  msg <- c(
    "!" = "{.pkg parsnip} could not locate an implementation for `{cls}` {mode} \\
           model specifications{if (isTRUE(user_specified_engine)) {
           paste0(' using the `', engine, '` engine')} else {''}}."
    )

  if (nrow(avail) == 0 && nrow(all) > 0) {
    pkgs <- unique(all$pkg)

    msg <-
      c(
        msg,
        "i" = paste0("{cli::qty(pkgs)}The parsnip extension package{?s} {.pkg {pkgs}}",
                     " implemen{?ts/t} support for this specification."),
        "i" = "Please install (if needed) and load to continue."
      )
  }

  prompt(c(msg, ""), ...)
}


#' Print the model call
#'
#' @param x A "model_spec" object.
#' @return A character string.
#' @keywords internal
#' @export
show_call <- function(object) {
  object$method$fit$args <-
    map(object$method$fit$args, convert_arg)

  call2(object$method$fit$func["fun"],
    !!!object$method$fit$args,
    .ns = object$method$fit$func["pkg"]
  )
}

convert_arg <- function(x) {
  if (is_quosure(x)) {
    quo_get_expr(x)
  } else {
    x
  }
}

levels_from_formula <- function(f, dat) {
  if (inherits(dat, "tbl_spark")) {
    res <- NULL
  } else {
    res <- levels(eval_tidy(f[[2]], dat))
  }
  res
}

#' @export
#' @keywords internal
#' @rdname add_on_exports
show_fit <- function(model, eng) {
  mod <- translate(x = model, engine = eng)
  fit_call <- show_call(mod)
  call_text <- deparse(fit_call)
  call_text <- paste0(call_text, collapse = "\n")
  paste0(
    "\\preformatted{\n",
    call_text,
    "\n}\n\n"
  )
}

# Check non-translated core arguments
# Each model has its own definition of this
check_args <- function(object) {
  UseMethod("check_args")
}

check_args.default <- function(object) {
  invisible(object)
}

# ------------------------------------------------------------------------------

# copied form recipes

names0 <- function(num, prefix = "x") {
  if (num < 1) {
    rlang::abort("`num` should be > 0.")
  }
  ind <- format(1:num)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}


# ------------------------------------------------------------------------------

#' @export
#' @keywords internal
#' @rdname add_on_exports
update_dot_check <- function(...) {
  dots <- enquos(...)

  if (length(dots) > 0) {
    rlang::abort(
      glue::glue(
        "Extra arguments will be ignored: ",
        glue::glue_collapse(glue::glue("`{names(dots)}`"), sep = ", ")
      )
    )
  }
  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @export
#' @keywords internal
#' @rdname add_on_exports
new_model_spec <- function(cls, args, eng_args, mode, user_specified_mode = TRUE,
                           method, engine, user_specified_engine = TRUE) {
  # determine if the model specification could feasibly match any entry
  # in the union of the parsnip model environment and model_info_table.
  # if not, trigger an error based on the (possibly inferred) model spec slots.
  out <- list(
    args = args, eng_args = eng_args,
    mode = mode, user_specified_mode = user_specified_mode, method = method,
    engine = engine, user_specified_engine = user_specified_engine
  )
  class(out) <- make_classes(cls)

  if (!spec_is_possible(spec = out)) {
    check_spec_mode_engine_val(cls, engine, mode, call = caller_env())
  }

  out
}

# ------------------------------------------------------------------------------

check_outcome <- function(y, spec) {
  if (spec$mode == "unknown") {
    return(invisible(NULL))
  }

  if (spec$mode == "regression") {
    outcome_is_numeric <- if (is.atomic(y)) {is.numeric(y)} else {all(map_lgl(y, is.numeric))}
    if (!outcome_is_numeric) {
      rlang::abort("For a regression model, the outcome should be numeric.")
    }
  }

  if (spec$mode == "classification") {
    outcome_is_factor <- if (is.atomic(y)) {is.factor(y)} else {all(map_lgl(y, is.factor))}
    if (!outcome_is_factor) {
      rlang::abort("For a classification model, the outcome should be a factor.")
    }
  }

  if (spec$mode == "censored regression") {
    outcome_is_surv <- inherits(y, "Surv")
    if (!outcome_is_surv) {
      rlang::abort("For a censored regression model, the outcome should be a `Surv` object.")
    }
  }

  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @export
#' @keywords internal
#' @rdname add_on_exports
check_final_param <- function(x) {
  if (is.null(x)) {
    return(invisible(x))
  }
  if (!is.list(x) & !tibble::is_tibble(x)) {
    rlang::abort("The parameter object should be a list or tibble")
  }
  if (tibble::is_tibble(x) && nrow(x) > 1) {
    rlang::abort("The parameter tibble should have a single row.")
  }
  if (tibble::is_tibble(x)) {
    x <- as.list(x)
  }
  if (length(names) == 0 || any(names(x) == "")) {
    rlang::abort("All values in `parameters` should have a name.")
  }

  invisible(x)
}

#' @export
#' @keywords internal
#' @rdname add_on_exports
update_main_parameters <- function(args, param) {
  if (length(param) == 0) {
    return(args)
  }
  if (length(args) == 0) {
    return(param)
  }

  # In case an engine argument is included:
  has_extra_args <- !(names(param) %in% names(args))
  extra_args <- names(param)[has_extra_args]
  if (any(has_extra_args)) {
    rlang::abort(
      paste(
        "At least one argument is not a main argument:",
        paste0("`", extra_args, "`", collapse = ", ")
      )
    )
  }
  param <- param[!has_extra_args]

  args <- utils::modifyList(args, param)
}

#' @export
#' @keywords internal
#' @rdname add_on_exports
update_engine_parameters <- function(eng_args, fresh, ...) {
  dots <- enquos(...)

  ## only update from dots when there are eng args in original model spec
  if (is_null(eng_args) || (fresh && length(dots) == 0)) {
    ret <- NULL
  } else {
    ret <- utils::modifyList(eng_args, dots)
  }

  has_extra_dots <- !(names(dots) %in% names(eng_args))
  dots <- dots[has_extra_dots]
  update_dot_check(!!!dots)

  ret
}

# ------------------------------------------------------------------------------
# Since stan changed the function interface
#' Wrapper for stan confidence intervals
#' @param object A stan model fit
#' @param newdata A data set.
#' @export
#' @keywords internal
stan_conf_int <- function(object, newdata) {
  check_installs(list(method = list(libs = "rstanarm")))
  if (utils::packageVersion("rstanarm") >= "2.21.1") {
    fn <- rlang::call2("posterior_epred",
      .ns = "rstanarm",
      object = expr(object),
      newdata = expr(newdata),
      seed = expr(sample.int(10^5, 1))
    )
  } else {
    fn <- rlang::call2("posterior_linpred",
      .ns = "rstanarm",
      object = expr(object),
      newdata = expr(newdata),
      transform = TRUE,
      seed = expr(sample.int(10^5, 1))
    )
  }
  rlang::eval_tidy(fn)
}

# ------------------------------------------------------------------------------

check_case_weights <- function(x, spec) {
  if (is.null(x) | spec$engine == "spark") {
    return(invisible(NULL))
  }
  if (!hardhat::is_case_weights(x)) {
    rlang::abort("'case_weights' should be a single numeric vector of class 'hardhat_case_weights'.")
  }
  allowed <- case_weights_allowed(spec)
  if (!allowed) {
    rlang::abort("Case weights are not enabled by the underlying model implementation.")
  }
  invisible(NULL)
}
