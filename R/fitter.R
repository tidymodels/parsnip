# Homogenous interfaces first

#' @importFrom  stats model.frame model.response terms as.formula model.matrix
form_form <-
  function(object, control, env, ...) {
    opts <- quos(...)
    
    fit_args <- object$method$fit$args
    
    if (is_spark(object)) {
      x <- data
      fit_args$x <- quote(x)
    } else {
      fit_args$data <- quote(data)
      fit_args$formula <- quote(formula)
    }

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
      lvl = levels_from_formula( # prob rewrite this as simple subset/levels
        env$formula,
        env$data
      ),
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
    class(res) <- "model_fit"
    res
  }

xy_xy <- function(object, env, control, target = "none", ...) {
  
  if (inherits(env$x, "tbl_spark") | inherits(env$y, "tbl_spark"))
    stop("spark objects can only be used with the formula interface to `fit`",
         call. = FALSE)
  
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
  class(res) <- "model_fit"
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
  class(res) <- "model_fit"
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

###################################################################
##

#' @name descriptors
#' @aliases descriptors n_obs n_cols n_preds n_facts n_levs
#' @title Data Set Characteristics Available when Fitting Models
#' @description When using the `fit` functions there are some
#'  variables that will be available for use in arguments. For
#'  example, if the user would like to choose an argument value
#'  based on the current number of rows in a data set, the `n_obs`
#'  variable can be used. See Details below.
#' @details
#' Existing variables:
#'   \itemize{
#'   \item `n_obs`: the current number of rows in the data set.
#'   \item `n_cols`: the number of columns in the data set that are
#'     associated with the predictors prior to dummy variable creation.
#'   \item `n_preds`: the number of predictors after dummy variables
#'     are created (if any).
#'   \item `n_facts`: the number of factor predictors in the dat set.
#'   \item `n_levs`: If the outcome is a factor, this is a table
#'     with the counts for each level (and `NA` otherwise)
#'   }
#'
#' For example, if you use the model formula `Sepal.Width ~ .` with the `iris`
#'  data, the values would be
#' \preformatted{
#'  n_cols  =   4     (the 4 columns in `iris`)
#'  n_preds =   5     (3 numeric columns + 2 from Species dummy variables)
#'  n_obs   = 150
#'  n_levs  =  NA     (no factor outcome)
#'  n_facts =   1     (the Species predictor)
#' }
#'
#' If the formula `Species ~ .` where used:
#' \preformatted{
#'  n_cols  =   4     (the 4 numeric columns in `iris`)
#'  n_preds =   4     (same)
#'  n_obs   = 150
#'  n_levs  =  c(setosa = 50, versicolor = 50, virginica = 50)
#'  n_facts =   0
#' }
#'
#' To use these in a model fit, either `expression` or `rlang::expr` can be
#' used to delay the evaluation of the argument value until the time when the
#' model is run via `fit` (and the variables listed above are available).
#' For example:
#'
#' \preformatted{
#' library(rlang)
#'
#' data("lending_club")
#'
#' rand_forest(mode = "classification", mtry = expr(n_cols - 2))
#' }
NULL

get_descr_form <- function(formula, data) {

  tmp_dat <- convert_form_to_xy_fit(formula, data, indicators = FALSE)
  
  if(is.factor(tmp_dat$y)) {
    n_levs <- table(tmp_dat$y, dnn = NULL)
  } else n_levs <- NA
  
  n_cols <- ncol(tmp_dat$x)
  n_preds <- ncol(convert_form_to_xy_fit(formula, data, indicators = TRUE)$x)
  n_obs <- nrow(data)
  n_facts <- sum(vapply(tmp_dat$x, is.factor, logical(1)))
  
  list(
    cols = n_cols,
    preds = n_preds,
    obs = n_obs,
    levs = n_levs,
    facts = n_facts
  )
}


get_descr_xy <- function(x, y) {
  if(is.factor(y)) {
    n_levs <- table(y, dnn = NULL)
  } else n_levs <- NA
  
  n_cols  <- ncol(x)
  n_preds <- ncol(x)
  n_obs   <- nrow(x)
  n_facts <- if(is.data.frame(x))
    sum(vapply(x, is.factor, logical(1)))
  else
    sum(apply(x, 2, is.factor)) # would this always be zero?
  
  list(
    cols = n_cols,
    preds = n_preds,
    obs = n_obs,
    levs = n_levs,
    facts = n_facts
  )
}

