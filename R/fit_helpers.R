# These functions are the go-betweens between parsnip::fit (or parsnip::fit_xy)
# and the underlying model function (such as ranger::ranger). So if `fit_xy()` is
# used to fit a ranger model, there needs to be a conversion from x/y format
# data to formula/data objects and so on.

form_form <-
  function(object, control, env, ...) {

    check_outcome(eval_tidy(env$formula[[2]], env$data), object)

    # prob rewrite this as simple subset/levels
    y_levels <- levels_from_formula(env$formula, env$data)
    object <- check_mode(object, y_levels)

    # if descriptors are needed, update descr_env with the calculated values
    if (requires_descrs(object)) {
      data_stats <- get_descr_form(env$formula, env$data)
      scoped_descrs(data_stats)
    }

    # evaluate quoted args once here to check them
    object <- check_args(object)

    # sub in arguments to actual syntax for corresponding engine
    object <- translate(object, engine = object$engine)

    fit_call <- make_form_call(object, env = env)

    res <- list(
      lvl = y_levels,
      spec = object
    )

    if (control$verbosity > 1L) {
      elapsed <- system.time(
        res$fit <- eval_mod(
          fit_call,
          capture = control$verbosity == 0,
          catch = control$catch,
          env = env,
          ...
        ),
        gcFirst = FALSE
      )
    } else {
      res$fit <- eval_mod(
        fit_call,
        capture = control$verbosity == 0,
        catch = control$catch,
        env = env,
        ...
      )
      elapsed <- list(elapsed = NA_real_)
    }
    res$preproc <- list(y_var = all.vars(env$formula[[2]]))
    res$elapsed <- elapsed
    res
  }

xy_xy <- function(object, env, control, target = "none", ...) {

  if (inherits(env$x, "tbl_spark") | inherits(env$y, "tbl_spark"))
    rlang::abort("spark objects can only be used with the formula interface to `fit()`")

  object <- check_mode(object, levels(env$y))
  check_outcome(env$y, object)

  encoding_info <-
    get_encoding(class(object)[1]) %>%
    dplyr::filter(mode == object$mode, engine == object$engine)

  remove_intercept <- encoding_info %>% dplyr::pull(remove_intercept)
  if (remove_intercept) {
    env$x <- env$x[, colnames(env$x) != "(Intercept)", drop = FALSE]
  }

  # if descriptors are needed, update descr_env with the calculated values
  if (requires_descrs(object)) {
    data_stats <- get_descr_xy(env$x, env$y)
    scoped_descrs(data_stats)
  }

  # evaluate quoted args once here to check them
  object <- check_args(object)

  # sub in arguments to actual syntax for corresponding engine
  object <- translate(object, engine = object$engine)

  fit_call <- make_xy_call(object, target)

  res <- list(lvl = levels(env$y), spec = object)

  if (control$verbosity > 1L) {
    elapsed <- system.time(
      res$fit <- eval_mod(
        fit_call,
        capture = control$verbosity == 0,
        catch = control$catch,
        env = env,
        ...
      ),
      gcFirst = FALSE
    )
  } else {
    res$fit <- eval_mod(
      fit_call,
      capture = control$verbosity == 0,
      catch = control$catch,
      env = env,
      ...
    )
    elapsed <- list(elapsed = NA_real_)
  }

  if (is.vector(env$y)) {
    y_name <- character(0)
  } else {
    y_name <- colnames(env$y)
  }
  res$preproc <- list(y_var = y_name)
  res$elapsed <- elapsed
  res
}

form_xy <- function(object, control, env,
                    target = "none", ...) {

  encoding_info <-
    get_encoding(class(object)[1]) %>%
    dplyr::filter(mode == object$mode, engine == object$engine)

  indicators <- encoding_info %>% dplyr::pull(predictor_indicators)
  remove_intercept <- encoding_info %>% dplyr::pull(remove_intercept)

  data_obj <- .convert_form_to_xy_fit(
    formula = env$formula,
    data = env$data,
    ...,
    composition = target,
    indicators = indicators,
    remove_intercept = remove_intercept
  )
  env$x <- data_obj$x
  env$y <- data_obj$y

  check_outcome(env$y, object)

  res <- xy_xy(
    object = object,
    env = env, #weights!
    control = control,
    target = target
  )
  data_obj$y_var <- all.vars(env$formula[[2]])
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

  encoding_info <-
    get_encoding(class(object)[1]) %>%
    dplyr::filter(mode == object$mode, engine == object$engine)

  remove_intercept <- encoding_info %>% dplyr::pull(remove_intercept)

  data_obj <-
    .convert_xy_to_form_fit(
      x = env$x,
      y = env$y,
      weights = NULL,
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
  if (is.vector(env$y)) {
    data_obj$y_var <- character(0)
  } else {
    data_obj$y_var <- colnames(env$y)
  }
  res$preproc <- data_obj[c("x_var", "y_var")]
  res
}

