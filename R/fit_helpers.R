# These functions are the go-betweens between parsnip::fit (or parsnip::fit_xy)
# and the underlying model function (such as ranger::ranger). So if `fit_xy` is
# used to fit a ranger model, there needs to be a conversion from x/y format
# data to formula/data objects and so on.

#' @importFrom  stats model.frame model.response terms as.formula model.matrix
form_form <-
  function(object, control, env, ...) {
    opts <- quos(...)

    y_levels <- levels_from_formula( # prob rewrite this as simple subset/levels
      env$formula,
      env$data
    )

    object <- check_mode(object, y_levels)

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

    # check to see of there are any `expr` in the arguments then
    # run a function that evaluates the data and subs in the
    # values of the expressions. we would have to evaluate the
    # formula (perhaps with and without dummy variables) to get
    # the appropraite number of columns. (`..vars..` vs `..cols..`)
    # Perhaps use `convert_form_to_xy_fit` here to get the results.

    if (make_descr(object)) {
      data_stats <- get_descr_form(env$formula, env$data)
      env$n_obs <- data_stats$obs
      env$n_cols <- data_stats$cols
      env$n_preds <- data_stats$preds
      env$n_levs <- data_stats$levs
      env$n_facts <- data_stats$facts
    }

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

  if (make_descr(object)) {
    data_stats <- get_descr_xy(env$x, env$y)
    env$n_obs <- data_stats$obs
    env$n_cols <- data_stats$cols
    env$n_preds <- data_stats$preds
    env$n_levs <- data_stats$levs
    env$n_facts <- data_stats$facts
  }

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
