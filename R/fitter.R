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
    
    if (is.name(formula))
      formula <- eval_tidy(formula)
  
    fit_args$formula <- formula
    
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
