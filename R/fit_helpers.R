# These functions are the go-betweens between parsnip::fit (or parsnip::fit_xy)
# and the underlying model function (such as ranger::ranger). So if `fit_xy()` is
# used to fit a ranger model, there needs to be a conversion from x/y format
# data to formula/data objects and so on.

#' @importFrom  stats model.frame model.response terms as.formula model.matrix
form_form <-
  function(object, control, env, ...) {

    if (object$mode == "classification") {
      # prob rewrite this as simple subset/levels
      y_levels <- levels_from_formula(env$formula, env$data)
      if (!inherits(env$data, "tbl_spark") && is.null(y_levels))
        rlang::abort("For classification models, the outcome should be a factor.")
    } else {
      y_levels <- NULL
    }

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

    fit_args <- object$method$fit$args

    if (is_spark(object)) {
      fit_args$x <- quote(x)
      env$x <- env$data
    } else {
      fit_args$data <- quote(data)
    }
    fit_args$formula <- quote(formula)

    fit_call <- make_call(
      fun = object$method$fit$func["fun"],
      ns = object$method$fit$func["pkg"],
      fit_args
    )

    res <- list(
      lvl = y_levels,
      spec = object
    )

    elapsed <- system.time(
      res$fit <- eval_mod(
        fit_call,
        capture = control$verbosity == 0,
        catch = control$catch,
        env = env,
        ...
      )
    )
    res$preproc <- list(y_var = all.vars(env$formula[[2]]))
    res$elapsed <- elapsed
    res
  }

xy_xy <- function(object, env, control, target = "none", ...) {

  if (inherits(env$x, "tbl_spark") | inherits(env$y, "tbl_spark"))
    rlang::abort("spark objects can only be used with the formula interface to `fit()`")

  object <- check_mode(object, levels(env$y))

  if (object$mode == "classification") {
    if (is.null(levels(env$y)))
      rlang::abort("For classification models, the outcome should be a factor.")
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

  object$method$fit$args[["y"]] <- quote(y)
  object$method$fit$args[["x"]] <-
    switch(
      target,
      none = quote(x),
      data.frame = quote(as.data.frame(x)),
      matrix = quote(as.matrix(x)),
      rlang::abort(glue::glue("Invalid data type target: {target}."))
    )

  fit_call <- make_call(
    fun = object$method$fit$func["fun"],
    ns = object$method$fit$func["pkg"],
    object$method$fit$args
  )

  res <- list(lvl = levels(env$y), spec = object)


  elapsed <- system.time(
    res$fit <- eval_mod(
      fit_call,
      capture = control$verbosity == 0,
      catch = control$catch,
      env = env,
      ...
    )
  )

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

  data_obj <- convert_form_to_xy_fit(
    formula = env$formula,
    data = env$data,
    ...,
    composition = target
    # indicators
  )
  env$x <- data_obj$x
  env$y <- data_obj$y

  res <- list(lvl = levels_from_formula(env$formula, env$data), spec = object)
  if (object$mode == "classification") {
    if (is.null(res$lvl))
      rlang::abort("For classification models, the outcome should be a factor.")
  }

  res <- xy_xy(
    object = object,
    env = env, #weights! offsets!
    control = control,
    target = target
  )
  data_obj$y_var <- all.vars(env$formula[[2]])
  data_obj$x <- NULL
  data_obj$y <- NULL
  data_obj$weights <- NULL
  data_obj$offset <- NULL
  res$preproc <- data_obj
  res
}

xy_form <- function(object, env, control, ...) {

  if (object$mode == "classification") {
    if (is.null(levels(env$y)))
      rlang::abort("For classification models, the outcome should be a factor.")
  }

  data_obj <-
    convert_xy_to_form_fit(
      x = env$x,
      y = env$y,
      weights = NULL,
      y_name = "..y"
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

