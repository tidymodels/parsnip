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
    cli::cli_abort(
      "Please pass other arguments to the model function via {.fun set_engine}."
    )
  }
  terms
}

is_missing_arg <- function(x) {
  identical(x, quote(missing_arg()))
}

# if the user specified an engine and the model object reflects that in
# the `user_specified_engine` slot, filter the model info down to
# those that the user specified. if not, don't filter the model info at all.
#
# note that, model objects generated pre parsnip 1.0.2, or from extensions
# that don't implement the `user_specified_engine` slot, will not trigger
# these checks.
engine_filter_condition <- function(engine, user_specified_engine, data) {
  # use !isTRUE so that result is TRUE if is.null(user_specified_engine)
  if (!isTRUE(user_specified_engine) || is.null(engine))  {
    return(TRUE)
  }

  data$engine == engine
}

# analogous helper for modes to `engine_filter_condition()`
mode_filter_condition <- function(mode, user_specified_mode, data) {
  # use !isTRUE so that result is TRUE if is.null(user_specified_mode)
  if (!isTRUE(user_specified_mode) || is.null(mode))  {
    return(TRUE)
  }

  data$mode == mode
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

  model_env <- rlang::env_get(get_model_env(), cls)
  model_env_matches <- model_env
  model_env_matches$model <- cls
  model_info_table_matches <-
    vctrs::vec_slice(model_info_table,
                     model_info_table$model == cls)

  if (isTRUE(user_specified_engine) && !is.null(engine)) {
    model_env_matches <-
      vctrs::vec_slice(model_env_matches,
                       model_env_matches$engine == engine)

    model_info_table_matches <-
      vctrs::vec_slice(model_info_table_matches,
                       model_info_table_matches$engine == engine)
  }

  if (isTRUE(user_specified_mode) && !is.null(mode)) {
    model_env_matches <-
      vctrs::vec_slice(model_env_matches,
                       model_env_matches$mode == mode)

    model_info_table_matches <-
      vctrs::vec_slice(model_info_table_matches,
                       model_info_table_matches$mode == mode)
  }


  if (vctrs::vec_size(model_env_matches) > 0 ||
      vctrs::vec_size(model_info_table_matches) > 0) {
    return(TRUE)
  }

  return(FALSE)
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

  avail <- get_from_env(cls)

  if (is.null(avail)) {
    return(FALSE)
  }

  engine_condition <- engine_filter_condition(engine, user_specified_engine, avail)
  mode_condition <- mode_filter_condition(mode, user_specified_mode, avail)

  avail <- avail |>
    vctrs::vec_slice(mode_condition & engine_condition)

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

  avail <- get_from_env(cls)

  engine_condition <- engine_filter_condition(engine, user_specified_engine, avail)
  mode_condition <- mode_filter_condition(mode, user_specified_mode, avail)

  if (!is.null(avail)) {
    avail <- vctrs::vec_slice(avail, mode_condition & engine_condition)
  }

  engine_condition_all <- engine_filter_condition(engine, user_specified_engine, model_info_table)
  mode_condition_all <- mode_filter_condition(mode, user_specified_mode, model_info_table)

  all <-
    vctrs::vec_slice(
      model_info_table,
      model_info_table$model == cls &
        mode_condition_all &
        engine_condition_all &
        !is.na(model_info_table$pkg)
    )

  all <- all[setdiff(names(all), "model")]

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
#' @param object A "model_spec" object.
#' @return A character string.
#' @keywords internal
#' @export
show_call <- function(object) {
  object$method$fit$args <- map(object$method$fit$args, convert_arg)

  fn_info <- as.list(object$method$fit$func)
  if (!any(names(fn_info) == "pkg")) {
    res <- call2(fn_info$fun, !!!object$method$fit$args)
  } else {
    res <- call2(fn_info$fun, !!!object$method$fit$args, .ns = fn_info$pkg)
  }
  res
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
    res <- list(lvls = NULL, ordered = FALSE)
  } else {
    res <- list()
    y_data <- eval_tidy(rlang::f_lhs(f), dat)
    res$lvls <- levels(y_data)
    res$ordered <- is.ordered(y_data)
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
#' @export
#' @keywords internal
#' @rdname add_on_exports
check_args <- function(object, call = rlang::caller_env()) {
  UseMethod("check_args")
}

#' @export
check_args.default <- function(object, call = rlang::caller_env()) {
  invisible(object)
}

# ------------------------------------------------------------------------------

# copied from recipes
# nocov start
names0 <- function(num, prefix = "x", call = rlang::caller_env()) {
  if (num < 1) {
    cli::cli_abort("{.arg num} should be > 0.", call = call)
  }
  ind <- format(seq_len(num))
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}
# nocov end

# ------------------------------------------------------------------------------

#' @export
#' @keywords internal
#' @rdname add_on_exports
update_dot_check <- function(...) {
  dots <- enquos(...)

  if (length(dots) > 0) {
    cli::cli_abort("The extra argument{?s} {.arg {names(dots)}} will be ignored.")
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

  has_no_outcome <- if (is.atomic(y)) {is.null(y)} else {length(y) == 0}
  if (isTRUE(has_no_outcome)) {
    cli::cli_abort(
      c("!" = "{.fun {class(spec)[1]}} was unable to find an outcome.",
        "i" = "Ensure that you have specified an outcome column and that it \\
               hasn't been removed in pre-processing."),
      call = NULL
    )
  }

  if (spec$mode == "regression") {
    outcome_is_numeric <- if (is.atomic(y)) {is.numeric(y)} else {all(map_lgl(y, is.numeric))}
    if (!outcome_is_numeric) {
      cli::cli_abort(
        "For a regression model, the outcome should be {.cls numeric}, not
        {.obj_type_friendly {y}}."
      )
    }
  }

  if (spec$mode == "classification") {
    outcome_is_factor <- if (is.atomic(y)) {is.factor(y)} else {all(map_lgl(y, is.factor))}
    if (!outcome_is_factor) {
      cli::cli_abort(
        "For a classification model, the outcome should be a {.cls factor}, not
        {.obj_type_friendly {y}}."
      )
    }

    if (inherits(spec, "logistic_reg") && is.atomic(y) && length(levels(y)) > 2) {
      # warn rather than error since some engines handle this case by binning
      # all but the first level as the non-event, so this may be intended
      cli::cli_warn(c(
        "!" = "Logistic regression is intended for modeling binary outcomes, \\
               but there are {length(levels(y))} levels in the outcome.",
        "i" = "If this is unintended, adjust outcome levels accordingly or \\
               see the {.fn multinom_reg} function."
      ))
    }
  }

  if (spec$mode == "censored regression") {
    outcome_is_surv <- inherits(y, "Surv")
    if (!outcome_is_surv) {
      cli::cli_abort(
        "For a censored regression model, the outcome should be a {.cls Surv} object, not
        {.obj_type_friendly {y}}."
      )
    }
  }

  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @export
#' @keywords internal
#' @rdname add_on_exports
check_final_param <- function(x, call = rlang::caller_env()) {
  if (is.null(x)) {
    return(invisible(x))
  }
  if (!is.list(x) & !tibble::is_tibble(x)) {
    cli::cli_abort("The parameter object should be a list or tibble.", call = call)
  }
  if (tibble::is_tibble(x) && nrow(x) > 1) {
    cli::cli_abort("The parameter tibble should have a single row.", call = call)
  }
  if (tibble::is_tibble(x)) {
    x <- as.list(x)
  }
  if (length(names) == 0 || any(names(x) == "")) {
    cli::cli_abort("All values in {.arg parameters} should have a name.", call = call)
  }

  invisible(x)
}

#' @export
#' @keywords internal
#' @rdname add_on_exports
update_main_parameters <- function(args, param, call = rlang::caller_env()) {
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
    cli::cli_abort(
      "Argument{?s} {.arg {extra_args}} {?is/are} not a main argument.",
      call = call
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

check_case_weights <- function(x, spec, call = rlang::caller_env()) {
  if (is.null(x) | spec$engine == "spark") {
    return(invisible(NULL))
  }
  if (!hardhat::is_case_weights(x)) {
    cli::cli_abort(
      "{.arg case_weights} should be a single numeric vector of
       class {.cls hardhat_case_weights}.",
      call = call
    )
  }
  allowed <- case_weights_allowed(spec)
  if (!allowed) {
    cli::cli_abort(
      "Case weights are not enabled by the underlying model implementation.",
      call = call
    )
  }
  invisible(NULL)
}

# ------------------------------------------------------------------------------

check_inherits <- function(x, cls, arg = caller_arg(x), call = caller_env()) {
  if (is.null(x)) {
    return(invisible(x))
  }

  if (!inherits(x, cls)) {
    cli::cli_abort(
      "{.arg {arg}} should be a {.cls {cls}}, not {.obj_type_friendly {x}}.",
      call = call
    )
  }
}

# -----------------------------------------------------------------------------
check_for_newdata <- function(..., call = rlang::caller_env()) {
  if (any(names(list(...)) == "newdata")) {
    cli::cli_abort(
      "Please use {.arg new_data} instead of {.arg newdata}.",
      call = call
    )
  }
}


# ------------------------------------------------------------------------------

# driven by https://github.com/Rdatatable/data.table/issues/5658
# nocov start
# adapted from ps:::is_cran_check()
is_cran_check <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    FALSE
  }
  else {
    Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""
  }
}
# nocov end

# ------------------------------------------------------------------------------

#' Obtain names of prediction columns for a fitted model or workflow
#'
#' [.get_prediction_column_names()] returns a list that has the names of the
#' columns for the primary prediction types for a model.
#' @param x A fitted parsnip model (class `"model_fit"`) or a fitted workflow.
#' @param syms Should the column names be converted to symbols? Defaults to `FALSE`.
#' @return A list with elements `"estimate"` and `"probabilities"`.
#' @examplesIf !parsnip:::is_cran_check() & rlang::is_installed("modeldata")
#' library(dplyr)
#' library(modeldata)
#' data("two_class_dat")
#'
#' levels(two_class_dat$Class)
#' lr_fit <- logistic_reg() |> fit(Class ~ ., data = two_class_dat)
#'
#' .get_prediction_column_names(lr_fit)
#' .get_prediction_column_names(lr_fit, syms = TRUE)
#' @export
.get_prediction_column_names <- function(x, syms = FALSE) {
  if (!inherits(x, c("model_fit", "workflow"))) {
    cli::cli_abort("{.arg x} should be an object with class {.cls model_fit} or
                    {.cls workflow}, not {.obj_type_friendly {x}}.")
  }

  if (inherits(x, "workflow")) {
    x <- x |> hardhat::extract_fit_parsnip(x)
  }
  model_spec <- extract_spec_parsnip(x)
  model_engine <- model_spec$engine
  model_mode <- model_spec$mode
  model_type <- class(model_spec)[1]

  # appropriate populate the model db
  inst_res <- purrr::map(required_pkgs(x), rlang::check_installed)
  predict_types <-
    get_from_env(paste0(model_type, "_predict")) |>
    dplyr::filter(engine == model_engine & mode == model_mode) |>
    purrr::pluck("type")

  if (length(predict_types) == 0) {
    cli::cli_abort("Prediction information could not be found for this
                   {.fn {model_type}} with engine {.val {model_engine}} and mode
                   {.val {model_mode}}. Does a parsnip extension package need to
                   be loaded?")
  }

  res <- list(estimate = character(0), probabilities = character(0))

  if (model_mode == "regression") {
    res$estimate <- ".pred"
  } else if (model_mode == "classification") {
    res$estimate <- ".pred_class"
    if (any(predict_types == "prob")) {
      res$probabilities <- paste0(".pred_", x$lvl)
    }
  } else if (model_mode == "censored regression") {
    res$estimate <- ".pred_time"
    if (any(predict_types %in% c("survival"))) {
      res$probabilities <- ".pred"
    }
  } else {
    # Should be unreachable
    cli::cli_abort("Unsupported model mode {model_mode}.")
  }

  if (syms) {
    res <- purrr::map(res, rlang::syms)
  }
  res
}
