#' Functions required for parsnip-adjacent packages
#'
#' These functions are helpful when creating new packages that will register
#' new model specifications.
#' @export
#' @keywords internal
#' @rdname add_on_exports
null_value <- function(x) {
  if (is_quosure(x)) {
    res <- isTRUE(all.equal(rlang::get_expr(x), expr(NULL)))
  } else {
    res <- isTRUE(all.equal(x, NULL))
  }
  res
}

check_eng_args <- function(args, obj, core_args) {
  # Make sure that we are not trying to modify an argument that
  # is explicitly protected in the method metadata or arg_key
  protected_args <- unique(c(obj$protect, core_args))
  common_args <- intersect(protected_args, names(args))
  if (length(common_args) > 0) {
    args <- args[!(names(args) %in% common_args)]
    cli::cli_warn(
      c(
        "The argument{?s} {.arg {common_args}} cannot be manually modified
         and {?was/were} removed."
      ),
      class = "parsnip_protected_arg_warning"
    )
  }
  args
}

#' Change elements of a model specification
#'
#' `set_args()` can be used to modify the arguments of a model specification while
#'  `set_mode()` is used to change the model's mode.
#'
#' @param object A [model specification][model_spec].
#' @param ... One or more named model arguments.
#' @param mode A character string for the model type (e.g. "classification" or
#'  "regression")
#' @return An updated model object.
#' @details `set_args()` will replace existing values of the arguments.
#'
#' @examplesIf !parsnip:::is_cran_check()
#' rand_forest()
#'
#' rand_forest() |>
#'   set_args(mtry = 3, importance = TRUE) |>
#'   set_mode("regression")
#'
#' linear_reg() |>
#'   set_mode("quantile regression", quantile_levels = c(0.2, 0.5, 0.8))
#' @export
set_args <- function(object, ...) {
  UseMethod("set_args")
}

#' @export
set_args.model_spec <- function(object, ...) {
  the_dots <- enquos(...)
  if (length(the_dots) == 0)
    cli::cli_abort("Please pass at least one named argument.")
  main_args <- names(object$args)
  new_args <- names(the_dots)
  for (i in new_args) {
    if (any(main_args == i)) {
      object$args[[i]] <- the_dots[[i]]
    } else {
      object$eng_args[[i]] <- the_dots[[i]]
    }
  }
  new_model_spec(
    cls = class(object)[1],
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    user_specified_mode = object$user_specified_mode,
    method = NULL,
    engine = object$engine,
    user_specified_engine = object$user_specified_engine
  )
}

#' @export
set_args.default <- function(object,...) {
  error_set_object(object, func = "set_args")

  invisible(FALSE)
}

#' @rdname set_args
#' @export
set_mode <- function(object, mode, ...) {
  UseMethod("set_mode")
}

#' @rdname set_args
#' @param quantile_levels A vector of values between zero and one (only for the
#' `"quantile regression"` mode); otherwise, it is `NULL`. The model uses these
#' values to appropriately train quantile regression models to make predictions
#' for these values (e.g., `quantile_levels = 0.5` is the median).
#' @export
set_mode.model_spec <- function(object, mode, quantile_levels = NULL, ...) {
  check_dots_empty()
  cls <- class(object)[1]
  if (rlang::is_missing(mode)) {
    spec_modes <- rlang::env_get(get_model_env(), paste0(cls, "_modes"))
    stop_incompatible_mode(spec_modes, cls = cls, call = caller_env(0))
  }

  # determine if the model specification could feasibly match any entry
  # in the union of the parsnip model environment and model_info_table.
  # if not, trigger an error based on the (possibly inferred) model spec slots.
  if (!spec_is_possible(spec = object,
                        mode = mode, user_specified_mode = TRUE)) {
    check_spec_mode_engine_val(cls, object$engine, mode)
  }

  object$mode <- mode
  object$user_specified_mode <- TRUE
  if (mode == "quantile regression") {
      hardhat::check_quantile_levels(quantile_levels)
  } else {
    if (!is.null(quantile_levels)) {
      cli::cli_warn("{.arg quantile_levels} is only used when the mode is
                     {.val quantile regression}.")
    }
  }

  object$quantile_levels <- quantile_levels
  object
}

#' @export
set_mode.default <- function(object, mode, ...) {
  error_set_object(object, func = "set_mode")

  invisible(FALSE)
}

# ------------------------------------------------------------------------------

maybe_eval <- function(x) {
  # if descriptors are in `x`, eval fails
  y <- try(rlang::eval_tidy(x), silent = TRUE)
  if (inherits(y, "try-error"))
    y <- x
  y
}

#' Evaluate parsnip model arguments
#' @export
#' @keywords internal
#' @param spec A [model specification][model_spec].
#' @param ... Not used.
eval_args <- function(spec, ...) {
  spec$args   <- purrr::map(spec$args,   maybe_eval)
  spec$eng_args <- purrr::map(spec$eng_args, maybe_eval)
  spec
}

# ------------------------------------------------------------------------------

# In some cases, a model function that we are calling has non-standard argument
# names. For example, a function foo() that only has the x/y interface might
# have a signature like `foo(X, Y)`.

# To deal with this, we allow for the `data` element of the model
# as an option to specify these actual argument names
#
#   value = list(
#     interface = "xy",
#     data = c(x = "X", y = "Y"),
#     protect = c("X", "Y"),
#     func = c(pkg = "bar", fun = "foo"),
#     defaults = list()
#   )

#' Make a parsnip call expression
#'
#' @param fun A character string of a function name.
#' @param ns A character string of a package name.
#' @param args A named list of argument values.
#' @details The arguments are spliced into the `ns::fun()` call. If they are
#' missing, null, or a single logical, then are not spliced.
#' @return A call.
#' @keywords internal
#' @export
make_call <- function(fun, ns, args, ...) {
  # remove any null or placeholders (`missing_args`) that remain
  discard <-
    vapply(args, function(x)
      is_missing_arg(x) | is.null(x), logical(1))
  args <- args[!discard]

  if (!is.null(ns) & !is.na(ns)) {
    out <- call2(fun, !!!args, .ns = ns)
  } else
    out <- call2(fun, !!!args)
  out
}


make_form_call <- function(object, env = NULL) {
  fit_args <- object$method$fit$args
  uses_weights <- has_weights(env)

  # In model specification code using `set_fit()`, there are two main arguments
  # that dictate the data-related model arguments (e.g. 'formula', 'data', 'x',
  # etc).
  # The 'protect' element specifies which data arguments should not be modifiable
  # by the user (as an engine argument). These have standardized names that
  # follow the usual R conventions. For example, `foo(formula, data, weights)`
  # and so on.
  # However, some packages do not follow these naming conventions. The 'data'
  # element in `set_fit()` allows use to have non-standard argument names by
  # providing a named list. If function `bar(f, dat, wts)` was being used, the
  # 'data' element would be `c(formula = "f", data = "dat", weights = "wts)`.
  # If conventional names are used, there is no 'data' element since the values
  # in 'protect' suffice.

  # Get the arguments related to data arguments to insert into the model call

  # Do we have conventional argument names?
  if (is.null(object$method$fit$data)) {
    # Set the minimum arguments for formula methods.
    data_args <- object$method$fit$protect
    names(data_args) <- data_args
    # Case weights _could_ be used but remove the arg if they are not given:
    if (!uses_weights) {
      data_args <- data_args[data_args != "weights"]
    }
  } else {
    # What are the non-conventional names?
    data_args <- object$method$fit$data
  }

  # add data arguments
  for (i in seq_along(data_args)) {
    fit_args[[ unname(data_args[i]) ]] <- sym(names(data_args)[i])
  }

  # sub in actual formula
  fit_args[[ unname(data_args["formula"]) ]]  <- env$formula

  # TODO remove weights col from data?
  if (object$engine == "spark") {
    env$x <- env$data
  }

  fit_call <- make_call(
    fun = object$method$fit$func["fun"],
    ns = object$method$fit$func["pkg"],
    fit_args
  )
  fit_call
}

# TODO we need something to indicate that case weights are being used.
make_xy_call <- function(object, target, env, call = rlang::caller_env()) {
  fit_args <- object$method$fit$args
  uses_weights <- has_weights(env)

  # See the comments above in make_form_call()

  if (is.null(object$method$fit$data)) {
    data_args <- object$method$fit$protect
    names(data_args) <- data_args
    # Case weights _could_ be used but remove the arg if they are not given:
    if (!uses_weights) {
      data_args <- data_args[data_args != "weights"]
    }
  } else {
    data_args <- object$method$fit$data
  }

  object$method$fit$args[[ unname(data_args["y"]) ]] <- rlang::expr(y)
  object$method$fit$args[[ unname(data_args["x"]) ]] <-
    switch(
      target,
      none = rlang::expr(x),
      data.frame = rlang::expr(maybe_data_frame(x)),
      matrix = rlang::expr(maybe_matrix(x)),
      dgCMatrix = rlang::expr(maybe_sparse_matrix(x)),
      cli::cli_abort("Invalid data type target: {target}.", call = call)
    )
  if (uses_weights) {
    object$method$fit$args[[ unname(data_args["weights"]) ]] <- rlang::expr(weights)
  }

  fit_call <- make_call(
    fun = object$method$fit$func["fun"],
    ns = object$method$fit$func["pkg"],
    object$method$fit$args
  )

  fit_call
}

## -----------------------------------------------------------------------------
#' Execution-time data dimension checks
#'
#' For some tuning parameters, the range of values depend on the data
#' dimensions (e.g. `mtry`). Some packages will fail if the parameter values are
#' outside of these ranges. Since the model might receive resampled versions of
#' the data, these ranges can't be set prior to the point where the model is
#' fit.  These functions check the possible range of the data and adjust them
#' if needed (with a warning).
#'
#' @param num_cols,num_rows The parameter value requested by the user.
#' @param source A data frame for the data to be used in the fit. If the source
#' is named "data", it is assumed that one column of the data corresponds to
#' an outcome (and is subtracted off).
#' @param offset A number subtracted off of the number of rows available in the
#' data.
#' @return An integer (and perhaps a warning).
#' @examplesIf !parsnip:::is_cran_check() & rlang::is_installed("kknn")  & rlang::is_installed("ranger")

#' nearest_neighbor(neighbors= 100) |>
#'   set_engine("kknn") |>
#'   set_mode("regression") |>
#'   translate()
#'
#' library(ranger)
#' rand_forest(mtry = 2, min_n = 100, trees = 3) |>
#'   set_engine("ranger") |>
#'   set_mode("regression") |>
#'   fit(mpg ~ ., data = mtcars)
#' @export
min_cols <- function(num_cols, source) {
  cl <- match.call()
  src_name <- rlang::expr_text(cl$source)
  if (cl$source == "data") {
    p <- ncol(source) - 1
  } else {
    p <- ncol(source)
  }
  if (num_cols > p) {
    cli::cli_warn(
      c(
        "!" = "{num_cols} column{?s} {?was/were} requested but there {cli::qty(p)} {?was/were}
               {p} predictor{?s} in the data.",
        "i" = "{p} predictor{?s} will be used."
      )
    )
    num_cols <- p
  }

  as.integer(num_cols)
}

#' @export
#' @rdname min_cols
min_rows <- function(num_rows, source, offset = 0) {
  if (inherits(source, "tbl_spark")) {
    n <- nrow_spark(source)
  } else {
    n <- nrow(source)
  }

  if (num_rows > n - offset) {
    cli::cli_warn(
      c(
        "!" = "{num_rows} sample{?s} {?was/were} requested but there were
               {n} rows in the data.",
        "i" = "{n - offset} sample{?s} will be used."
      )
    )

    num_rows <- n - offset
  }

  as.integer(num_rows)
}

nrow_spark <- function(source) {
  rlang::check_installed("sparklyr")
  sparklyr::sdf_nrow(source)
}
