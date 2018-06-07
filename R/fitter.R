# Homogenous interfaces first

#' @importFrom  stats model.frame model.response terms as.formula model.matrix
form_form <-
  function(object, formula, data, control, ...) {
    opts <- quos(...)
    
    fit_args <- object$method$fit_args
    
    if (is_spark(object)) {
      x <- data
      fit_args$x <- quote(x)
    } else {
      if (is.name(data) | is.call(data))
        fit_args$data <- data
      else
        fit_args$data <- quote(data)
    }
    
    tmp_data <- eval_tidy(data)
    
    if (is.name(formula))
      formula <- eval_tidy(formula)
  
    fit_args$formula <- formula
    
    # check to see of there are any `expr` in the arguments then
    # run a function that evaluates the data and subs in the 
    # values of the expressions. we would have to evaluate the 
    # formula (perhaps with and without dummy variables) to get
    # the appropraite number of columns. (`..vars..` vs `..cols..`)
    # Perhaps use `convert_form_to_xy_fit` here to get the results.

    data_stats <- get_descr_form(formula, data)
    n_obs <- data_stats$obs
    n_cols <- data_stats$cols
    n_preds <- data_stats$preds
    n_lev <- data_stats$lev
    n_fact <- data_stats$fact
    
    fit_call <- make_call(
      fun = object$method$fit_name["fun"],
      ns = object$method$fit_name["pkg"],
      fit_args
    )
    
    res <- list(
      lvl = levels_from_formula(
        formula, 
        eval_tidy(data)
      ), 
      spec = object
    )
    
    res$fit <- eval_mod(
      fit_call,
      capture = control$verbosity == 0,
      catch = control$catch,
      env = current_env(),
      ...
    )
    res$preproc <- NA
    class(res) <- "model_fit"
    res
  }

xy_xy <- function(object, x, y, control, target = "none", ...) {
  
  if (inherits(x, "tbl_spark") | inherits(y, "tbl_spark"))
    stop("spark objects can only be used with the formula interface to `fit`",
         call. = FALSE)
  
  object$method$fit_args[["y"]] <- quote(y)
  object$method$fit_args[["x"]] <- 
    switch(
      target,
      none = quote(x),
      data.frame = quote(as.data.frame(x)),
      matrix = quote(as.matrix(x)),
      stop("Invalid data type target: ", target)
    )

  data_stats <- get_descr_xy(x, y)
  n_obs <- data_stats$obs
  n_cols <- data_stats$cols
  n_preds <- data_stats$preds
  n_lev <- data_stats$lev
  n_fact <- data_stats$fact
  
  fit_call <- make_call(
    fun = object$method$fit_name["fun"],
    ns = object$method$fit_name["pkg"],
    object$method$fit_args
  )
  
  res <- list(lvl = levels(y), spec = object)
  
  res$fit <- eval_mod(
    fit_call,
    capture = control$verbosity == 0,
    catch = control$catch,
    env = current_env(),
    ...
  )
  res$preproc <- NA
  class(res) <- "model_fit"
  res
}

form_xy <- function(object, formula, data, control, 
                    target = "none", ...) {
  data_obj <- convert_form_to_xy_fit(
    formula = formula,
    data = data, 
    ...,
    composition = target
    # indicators
  )
  res <- list(
    lvl = levels_from_formula(
      formula, 
      eval_tidy(data)
    ), 
    spec = object
  )
  
  res$fit <- xy_xy(
    object = object, 
    x = data_obj$x, 
    y = data_obj$y, #weights! offsets!
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

xy_form <- function(object, x, y, control, ...) {
  data_obj <-
    convert_xy_to_form_fit(
      x = x,
      y = y,
      weights = NULL,
      y_name = "..y"
    )
  # which terms etc goes in the preproc slot here?
  res <- form_form(
    object = object, 
    formula = data_obj$formula,
    data = data_obj$data, 
    control = control,
    ...
  )
  res$preproc <- data_obj[["x_var"]]
  res
}

###################################################################
##

# do something for outcome factor level counts too? 
# n_fact, num_num
get_descr_form <- function(formula, data) {
  if(is.name(formula))
    formula <- eval_tidy(formula)
  if(is.name(data))
    data <- eval_tidy(data)  
  
  tmp_dat <- convert_form_to_xy_fit(formula, data, indicators = FALSE)
  
  if(is.factor(tmp_dat$y)) {
    n_lev <- table(tmp_dat$y)
  } else n_lev <- NA
  
  n_cols <- ncol(tmp_dat$x)
  n_preds <- ncol(convert_form_to_xy_fit(formula, data, indicators = TRUE)$x)
  n_obs <- nrow(data)
  n_fact <- sum(vapply(tmp_dat$x, is.factor, logical(1)))
  
  list(
    cols = n_cols,
    preds = n_preds,
    obs = n_obs,
    lev = n_lev,
    fact = n_fact
  )
}


get_descr_xy <- function(x, y) {
  if(is.name(x))
    x <- eval_tidy(x)
  if(is.name(y))
    x <- eval_tidy(y) 
  
  if(is.factor(y)) {
    n_lev <- table(y)
  } else n_lev <- NA
  
  n_cols  <- ncol(x)
  n_preds <- ncol(x)
  n_obs   <- nrow(x)
  n_fact <- if(is.data.frame(x)) 
    sum(vapply(x, is.factor, logical(1)))
  else 
    sum(apply(x, 2, is.factor)) # would this always be zero?
  
  list(
    cols = n_cols,
    preds = n_preds,
    obs = n_obs,
    lev = n_lev,
    fact = n_fact
  )
}

