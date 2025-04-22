# These functions are the go-betweens between parsnip::fit (or parsnip::fit_xy)
# and the underlying model function (such as ranger::ranger). So if `fit_xy()` is
# used to fit a ranger model, there needs to be a conversion from x/y format
# data to formula/data objects and so on.

form_form <-
  function(object, control, env, ..., call = rlang::caller_env()) {

    if (inherits(env$data, "data.frame")) {
      check_outcome(eval_tidy(rlang::f_lhs(env$formula), env$data), object)

      encoding_info <- get_encoding(class(object)[1])
      encoding_info <-
        vctrs::vec_slice(
          encoding_info,
          encoding_info$mode == object$mode & encoding_info$engine == object$engine
        )

      remove_intercept <- encoding_info |> dplyr::pull(remove_intercept)
      if (remove_intercept) {
        env$data <- env$data[, colnames(env$data) != "(Intercept)", drop = FALSE]
      }
    }

    # prob rewrite this as simple subset/levels
    y_levels <- levels_from_formula(env$formula, env$data)

    # if descriptors are needed, update descr_env with the calculated values
    if (requires_descrs(object)) {
      data_stats <- get_descr_form(env$formula, env$data, call = call)
      scoped_descrs(data_stats)
    }

    # evaluate quoted args once here to check them
    object <- check_args(object, call = call)

    # sub in arguments to actual syntax for corresponding engine
    object <- translate(object, engine = object$engine)

    fit_call <- make_form_call(object, env = env)

    res <- list(
      lvl = y_levels$lvl,
      ordered = y_levels$ordered,
      spec = object
    )

    time <- proc.time()
    res$fit <- eval_mod(
      fit_call,
      capture = control$verbosity == 0,
      catch = control$catch,
      envir = env,
      ...
    )
    elapsed <- proc.time() - time
    res$preproc <- list(y_var = all.vars(rlang::f_lhs(env$formula)))
    res$elapsed <- list(elapsed = elapsed, print = control$verbosity > 1L)

    res
  }

xy_xy <- function(object,
                  env,
                  control,
                  target = "none",
                  ...,
                  call = rlang::caller_env()) {

  if (inherits(env$x, "tbl_spark") | inherits(env$y, "tbl_spark")) {
    cli::cli_abort(
      "spark objects can only be used with the formula interface to {.fun fit}.",
      call = call
    )
  }

  check_outcome(env$y, object)

  encoding_info <-
    get_encoding(class(object)[1]) |>
    dplyr::filter(mode == object$mode, engine == object$engine)

  remove_intercept <- encoding_info |> dplyr::pull(remove_intercept)
  if (remove_intercept) {
    env$x <- env$x[, colnames(env$x) != "(Intercept)", drop = FALSE]
  }

  # if descriptors are needed, update descr_env with the calculated values
  if (requires_descrs(object)) {
    data_stats <- get_descr_xy(env$x, env$y, call = call)
    scoped_descrs(data_stats)
  }

  # evaluate quoted args once here to check them
  object <- check_args(object, call = call)

  # sub in arguments to actual syntax for corresponding engine
  object <- translate(object, engine = object$engine)

  fit_call <- make_xy_call(object, target, env, call)

  res <- list(lvl = levels(env$y), ordered = is.ordered(env$y), spec = object)

  time <- proc.time()
  res$fit <- eval_mod(
    fit_call,
    capture = control$verbosity == 0,
    catch = control$catch,
    envir = env,
    ...
  )
  elapsed <- proc.time() - time

  if (is.atomic(env$y)) {
    y_name <- character(0)
  } else {
    y_name <- colnames(env$y)
  }
  res$preproc <- list(y_var = y_name, x_names = colnames(env$x))
  res$elapsed <- list(elapsed = elapsed, print = control$verbosity > 1L)
  res
}

form_xy <- function(object, control, env,
                    target = "none", ..., call = rlang::caller_env()) {

  encoding_info <-
    get_encoding(class(object)[1]) |>
    dplyr::filter(mode == object$mode, engine == object$engine)

  indicators <- encoding_info |> dplyr::pull(predictor_indicators)
  remove_intercept <- encoding_info |> dplyr::pull(remove_intercept)
  allow_sparse_x <- encoding_info |> dplyr::pull(allow_sparse_x)

  if (allow_sparse_x && sparsevctrs::has_sparse_elements(env$data)) {
    target <- "dgCMatrix"
  }

  data_obj <- .convert_form_to_xy_fit(
    formula = env$formula,
    data = env$data,
    ...,
    composition = target,
    indicators = indicators,
    remove_intercept = remove_intercept,
    call = call
  )
  env$x <- data_obj$x
  env$y <- data_obj$y

  res <- xy_xy(
    object = object,
    env = env, #weights!
    control = control,
    target = target,
    call = call
  )
  data_obj$y_var <- all.vars(rlang::f_lhs(env$formula))
  data_obj$x <- NULL
  data_obj$y <- NULL
  data_obj$weights <- NULL
  # TODO: Should we be using the offset that we remove here?
  data_obj$offset <- NULL
  res$preproc <- data_obj
  res
}

xy_form <- function(object, env, control, ...) {

  check_outcome(env$y, object)

  encoding_info <- get_encoding(class(object)[1])
  encoding_info <-
    vctrs::vec_slice(
      encoding_info,
      encoding_info$mode == object$mode & encoding_info$engine == object$engine
    )

  remove_intercept <- encoding_info$remove_intercept

  data_obj <-
    .convert_xy_to_form_fit(
      x = env$x,
      y = env$y,
      weights = env$weights,
      y_name = "..y",
      remove_intercept = remove_intercept
    )
  env$formula <- data_obj$formula
  env$data <- data_obj$data

  # which terms etc goes in the preproc slot here?
  res <- form_form(
    object = object,
    env = env,
    control = control,
    ...
  )
  if (!is.null(env$y_var)) {
    data_obj$y_var <- env$y_var
  } else {
    if (is.atomic(env$y)) {
      data_obj$y_var <- character(0)
    }

    data_obj$y_var <- colnames(env$y)
  }

  res$preproc <- data_obj[c("x_var", "y_var")]
  res
}
