# The "fit_interface" is what was supplied to `fit` as defined by
#  `check_interface`. The "model interface" is what the underlying
#  model uses. These functions go from one to another.

# TODO return pp objects like terms or recipe

# TODO protect engine = "spark" with non-spark data object

# TODO formula method and others pass symbols like formula method

fit_interface_matrix <- function(x, y, object, control, ...) {
  if (object$engine == "spark")
    stop("spark objects can only be used with the formula interface to `fit` ",
         "with a spark data object.", call. = FALSE)
  switch(
    object$method$interface,
    data.frame = matrix_to_data.frame(object, x, y, control, ...),
    matrix =         matrix_to_matrix(object, x, y, control, ...),
    formula =       matrix_to_formula(object, x, y, control, ...),
    stop("I don't know about model interface '",
         object$method$interface, "'.", call. = FALSE)
    )
}

fit_interface_data.frame <- function(x, y, object, control, ...) {
  if (object$engine == "spark")
    stop("spark objects can only be used with the formula interface to `fit` ",
         "with a spark data object.", call. = FALSE)
  switch(
    object$method$interface,
    data.frame = data.frame_to_data.frame(object, x, y, control, ...),
    matrix =         data.frame_to_matrix(object, x, y, control, ...),
    formula =       data.frame_to_formula(object, x, y, control, ...),
    stop("I don't know about model interface '",
         object$method$interface, "'.", call. = FALSE)
  )
}

fit_interface_formula <- function(formula, data, object, control, ...) {
  switch(
    object$method$interface,
    data.frame = formula_to_data.frame(object, formula, data, control, ...),
    matrix =         formula_to_matrix(object, formula, data, control, ...),
    formula =       formula_to_formula(object, formula, data, control, ...),
    stop("I don't know about model interface '",
         object$method$interface, "'.", call. = FALSE)
  )
}

fit_interface_recipe <- function(recipe, data, object, control, ...) {
  if (object$engine == "spark")
    stop("spark objects can only be used with the formula interface to `fit` ",
         "with a spark data object.", call. = FALSE)
  switch(
    object$method$interface,
    data.frame = recipe_to_data.frame(object, recipe, data, control, ...),
    formula = recipe_to_formula(object, recipe, data, control, ...),
    matrix = recipe_to_matrix(object, recipe, data, control, ...),
    stop("I don't know about model interface '",
         object$method$interface, "'.", call. = FALSE)
  )
}

###################################################################
## starts with some x/y interface (either matrix or data frame)
## in `fit`

#' @importFrom dplyr bind_cols

xy_to_xy <- function(object, x, y, control, ...) {

  if (inherits(x, "tbl_spark") | inherits(y, "tbl_spark"))
    stop("spark objects can only be used with the formula interface to `fit`",
         call. = FALSE)

  object$method$fit_args[["x"]] <- quote(x)
  object$method$fit_args[["y"]] <- quote(y)

  fit_call <- make_call(
    fun = object$method$fit_name["fun"],
    ns = object$method$fit_name["pkg"],
    object$method$fit_args
  )

  eval_mod(
    fit_call,
    capture = control$verbosity == 0,
    catch = control$catch,
    env = current_env(),
    ...
  )
}

data.frame_to_data.frame <- function(object, x, y, control, ...) {
  if (!is.data.frame(x))
    x <- as.data.frame(x)
  if (is.matrix(y) && isTRUE(ncol(y) > 1))
    y <- as.data.frame(y)
  xy_to_xy(object, x, y, control, ...)
}

matrix_to_matrix <- function(object, x, y, control, ...) {
  if (!is.matrix(x))
    x <- as.matrix(x)
  if (!is.matrix(y) && isTRUE(ncol(y) > 1))
    y <- as.matrix(y)
  xy_to_xy(object, x, y, control, ...)
}

data.frame_to_matrix <- function(object, x, y, control, ...) {
  matrix_to_matrix(object, x, y, control, ...)
}

matrix_to_data.frame <- function(object, x, y, control, ...) {
  data.frame_to_data.frame(object, x, y, control, ...)
}

matrix_to_formula <- function(object, x, y, control, ...) {
  if (!is.data.frame(x))
    x <- as.data.frame(x)

  # bind y to x and make formula
  if (is.data.frame(y) | is.matrix(y)) {
    if (ncol(y) == 1) {
      x$.outcome <- y[, 1]
      form <- .outcome ~ .
    } else {
      if (!is.data.frame(y))
        x <- as.data.frame(y)
      x <- bind_cols(x, y)
      form <- paste0("cbind(", paste0(colnames(y), collapse = ","), ")~.")
    }
  } else {
    x$.outcome <- y
    form <- .outcome ~ .
  }
  formula_to_formula(object, formula = form, data = x, control, ...)
}

data.frame_to_formula <- function(object, x, y, control, ...) {
  matrix_to_formula(object, x, y, control, ...)
}

###################################################################
## Start with formula interface in `fit`

#' @importFrom  stats model.frame model.response terms as.formula model.matrix

formula_to_formula <-
  function(object, formula, data, control, ...) {
    opts <- quos(...)

    fit_args <- object$method$fit_args

    if (isTRUE(unname(object$method$fit_name["pkg"] == "sparklyr"))) {
      x <- data
      fit_args$x <- quote(x)
    } else {
      if (is.name(data) | is.call(data))
        fit_args$data <- data
      else
        fit_args$data <- quote(data)
    }
    fit_args$formula <- formula

    fit_call <- make_call(fun = object$method$fit_name["fun"],
                          ns = object$method$fit_name["pkg"],
                          fit_args)

    res <-
      eval_mod(
        fit_call,
        capture = control$verbosity == 0,
        catch = control$catch,
        env = current_env(),
        ...
      )
    res
  }

formula_to_data.frame <- function(object, formula, data, control, ...) {
  if (is.name(data))
    data <- eval_tidy(data, env = caller_env())

  if (!is.data.frame(data))
    data = as.data.frame(data)

  # TODO: how do we fill in the other standard things here (subset, contrasts etc)?

  x <- stats::model.frame(eval(formula), eval(data))
  y <- model.response(x)

  # Remove outcome column(s) from `x`
  outcome_cols <- attr(terms(x), "response")
  if (!isTRUE(all.equal(outcome_cols, 0))) {
    x <- x[, -outcome_cols, drop = FALSE]
  }
  xy_to_xy(object, x, y, control, ...)
}

formula_to_matrix <- function(object, formula, data, control, ...) {
  if (is.name(data))
    data <- eval_tidy(data, env = caller_env())

  if (!is.data.frame(data))
    data = as.data.frame(data)

  # TODO: how do we fill in the other standard things here (subset, etc)?

  x <- stats::model.frame(eval(formula), eval(data))
  trms <- attr(x, "terms")
  y <- model.response(x)
  if (is.data.frame(y))
    y <- as.matrix(y)

  # TODO sparse model matrices?
  x <- model.matrix(trms, data = x, contrasts.arg = getOption("contrasts"))
  # TODO Assume no intercept for now
  x <- x[, !(colnames(x) %in% "(Intercept)"), drop = FALSE]

  xy_to_xy(object, x, y, control, ...)
}

###################################################################
## Start with recipe interface in `fit`

#' @importFrom recipes prep juice all_predictors all_outcomes

# add case weights as extra object returned (out$weights)
recipe_data <- function(recipe, data, object, control, output = "matrix", combine = FALSE) {
  recipe <-
    prep(recipe, training = data, retain = TRUE, verbose = control$verbosity > 1)

  if (combine) {
    out <- list(
      data = juice(recipe, composition = output),
      form = formula(object, (recipe))
    )

  } else {
    out <-
      list(
        x = juice(recipe, all_predictors(), composition = output),
        y = juice(recipe, all_outcomes(), composition = output)
      )
    if (ncol(out$y) == 1) {
      if (is.matrix(out$y))
        out$y <- out$y[, 1]
      else
        out$y <- out$y[[1]]
    }

  }
  out
}

recipe_to_formula <-
  function(object, recipe, data, control, ...) {
    info <- recipe_data(recipe, data, object, control, output = "tibble", combine = TRUE)
    formula_to_formula(object, info$form, info$data, control, ...)
  }

recipe_to_data.frame <- function(object, recipe, data, control, ...) {
  info <- recipe_data(recipe, data, object, control, output = "tibble", combine = FALSE)
  xy_to_xy(object, info$x, info$y, control, ...)
}

recipe_to_matrix <- function(object, recipe, data, control, ...) {
  info <- recipe_data(recipe, data, object, control, output = "matrix", combine = FALSE)
  xy_to_xy(object, info$x, info$y, control, ...)
}
