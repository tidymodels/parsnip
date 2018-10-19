# These functions are the go-betweens between parsnip::fit (or parsnip::fit_xy)
# and the underlying model function (such as ranger::ranger). So if `fit_xy` is
# used to fit a ranger model, there needs to be a conversion from x/y format
# data to formula/data objects and so on.

#' @importFrom  stats model.frame model.response terms as.formula model.matrix
form_form <-
  function(object, control, env, ...) {
    opts <- quos(...)

    if (object$mode != "regression") {
      y_levels <- levels_from_formula( # prob rewrite this as simple subset/levels
        env$formula,
        env$data
      )
    } else {
      y_levels <- NULL
    }

    object <- check_mode(object, y_levels)

    # if descriptors are needed, update descr_env with the calculated values
    if(requires_descrs(object)) {
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

    res$fit <- eval_mod(
      fit_call,
      capture = control$verbosity == 0,
      catch = control$catch,
      env = env,
      ...
    )
    res$preproc <- NA
    res
  }

xy_xy <- function(object, env, control, target = "none", ...) {

  if (inherits(env$x, "tbl_spark") | inherits(env$y, "tbl_spark"))
    stop("spark objects can only be used with the formula interface to `fit`",
         call. = FALSE)

  object <- check_mode(object, levels(env$y))

  # if descriptors are needed, update descr_env with the calculated values
  if(requires_descrs(object)) {
    data_stats <- get_descr_form(env$formula, env$data)
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
      stop("Invalid data type target: ", target)
    )

  fit_call <- make_call(
    fun = object$method$fit$func["fun"],
    ns = object$method$fit$func["pkg"],
    object$method$fit$args
  )

  res <- list(lvl = levels(env$y), spec = object)

  res$fit <- eval_mod(
    fit_call,
    capture = control$verbosity == 0,
    catch = control$catch,
    env = env,
    ...
  )
  res$preproc <- NA
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

  res <- list(
    lvl = levels_from_formula(
      env$formula,
      env$data
    ),
    spec = object
  )

  res <- xy_xy(
    object = object,
    env = env, #weights! offsets!
    control = control,
    target = target
  )
  data_obj$x <- NULL
  data_obj$y <- NULL
  data_obj$weights <- NULL
  data_obj$offset <- NULL
  res$preproc <- data_obj
  res
}

xy_form <- function(object, env, control, ...) {
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
  res$preproc <- data_obj["x_var"]
  res
}

# ------------------------------------------------------------------------------
##


#' @importFrom utils globalVariables
utils::globalVariables("data")
