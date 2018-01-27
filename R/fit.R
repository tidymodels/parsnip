# General TODOs
# - think about case weights in each instance below
# - try/catch all model fit evaluations
# - option to capture output/verboseness



#' Fit a Model Specification to a Dataset
#' 
#' `fit` will take a model specification, finalize the required
#'  code by substituting arguments, and execute the model fit
#'  routine.
#' 
#' @param object An object of class `model_spec`
#' @export
#' @rdname fit 
fit <- function (object, ...) 
  UseMethod("fit")

# The S3 part here is awful (for now I hope)

#' @importFrom utils capture.output
# fit_formula <- function(object, formula, data, verboseness = 0, engine = "ranger") {
#   varying_param_check(object)
# 
#   # go between input methods
# 
#   # data checks based on method
# 
#   object <- finalize(object, engine = engine)
#   if(verboseness == 0) {
#     fit_obj <- eval(object$method$fit)
#   } else {
#     capture.output(fit_obj <- eval(object$method$fit))
#   }
#   fit_obj
# }


#' @export
fit.model_spec <- function(object, x, engine = object$engine, ...) {
  object$engine <- engine
  object <- check_engine(object)
  
  # sub in arguments to actual syntax for corresponding engine
  object <- finalize(object, engine = object$engine)
  check_installs(object)
  
  if (inherits(x, "formula")) {
    res <- fit_formula(object, formula = x, ...)
  } else {
    if (inherits(x, c("matrix", "data.frame"))) {
      res <- fit_xy(object, x = x, ...)
    } else {
      if (inherits(x, "recipe")) {
        res <- fit_recipe(object, recipe = x, ...)
      } else {
        stop("`x` should be a formula, data frame, matrix, or recipe")
      }
    }
  }
  res
}

###################################################################

#' @importFrom rlang eval_tidy quos 
#' @importFrom stats as.formula
fit_formula <- function(object, formula, engine = engine, ...) {
  opts <- quos(...)

  if(!any(names(opts) == "data"))
    stop("Please pass a data frame with the `data` argument.",
         call. = FALSE)
  
  # TODO Should probably just load the namespace
  for(pkg in object$method$library)
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))

  # Look up the model's interface (e.g. formula, recipes, etc) 
  # and delagate to the connector functions (`formula_to_recipe` etc)
  if(object$method$interface == "formula") {
    fit_expr <- sub_arg_values(object$method$fit, opts["data"])
    fit_expr$formula <- as.formula(eval(formula))
    res <- rlang:::eval_tidy(fit_expr)
  } else {
    if(object$method$interface %in% c("data.frame", "matrix")) {
      res <- formula_to_xy(object = object, formula = formula, data = opts["data"])
    } else {
      stop("I don't know about the ", 
           object$method$interface, " interface.",
           call. = FALSE)
    }
  }
  res
}

fit_xy <- function(object, x, ...) {
  # Look up the model's interface (e.g. formula, recipes, etc) 
  # and delagate to the connector functions (`xy_to_formula` etc)
  opts <- quos(...)
  
  if(!any(names(opts) == "y"))
    stop("Please pass a data frame with the `y` argument.",
         call. = FALSE)
  
  # TODO Should probably just load the namespace
  for(pkg in object$method$library)
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  
  # Look up the model's interface (e.g. formula, recipes, etc) 
  # and delagate to the connector functions (`xy_to_formula` etc)
  if(object$method$interface == "formula") {
    res <- xy_to_formula(object = object, x = x, y = opts["y"])
  } else {
    if(object$method$interface %in% c("data.frame", "matrix")) {
      fit_expr <- sub_arg_values(object$method$fit, opts["y"])
      res <- rlang:::eval_tidy(fit_expr)
    } else {
      stop("I don't know about the ", 
           object$method$interface, " interface.",
           call. = FALSE)
    }
  }
  res  
}

fit_recipe <- function(object, recipe, ...) {
  # Look up the model's interface (e.g. formula, recipes, etc) 
  # and delagate to the connector functions (`recipe_to_formula` etc)
}

###################################################################

formula_to_recipe <- function(object, formula, data) {
  # execute the formula
  # extract terms _and roles_
  # put into recipe
  
}

#' @importFrom  stats model.frame
formula_to_xy <- function(object, formula, data) {
  # TODO how do we fill in the other standard things here (subset, contrasts etc)?
  # TODO add a "matrix" option here and invoke model.matrix
  
  # Q: avoid eval using ?rlang:::get_expr(data[["data"]])
  x <- stats::model.frame(formula, eval_tidy(data[["data"]]))
  y <- model.response(x, "numeric")
  eval_tidy(object$method$fit)
}

###################################################################

recipe_to_formula <- function(object, recipe, data) {

}

recipe_to_xy <- function(object, recipe, data) {

}

###################################################################

xy_to_formula <- function(object, x, y) {
  if(!is.data.frame(x))
    x <- as.data.frame(x)
  x$.y <- eval_tidy(y[["y"]])
  fit_expr <- object$method$fit
  fit_expr$formula <- as.formula(.y ~ .)
  fit_expr$data <- quote(x)
  rlang:::eval_tidy(fit_expr)
}

xy_to_recipe <- function(object, x, y) {
  
}




