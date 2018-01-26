
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

# The S3 part here is awful for now

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
      res <- fit_xy(object, formula = x, ...)
    } else {
      if (inherits(x, "recipe")) {
        res <- fit_recipe(object, formula = x, ...)
      } else {
        stop("`x` should be a formula, data frame, matrix, or recipe")
      }
    }
  }
  res
}

###################################################################

fit_formula <- function(object, formula = x, engine = engine, ...) {
  opts <- quos(...)
  if(!any(names(opts) == "data"))
    stop("Please pass a data frame with the `data` argument.",
         call. = FALSE)

  # Look up the model's interface (e.g. formula, recipes, etc) 
  # and delagate to the connector functions (`formula_to_recipe` etc)
  
}

fit_xy <- function(object, formula = x, ...) {
  # Look up the model's interface (e.g. formula, recipes, etc) 
  # and delagate to the connector functions (`xy_to_formula` etc)
}

fit_recipe <- function(object, formula = x, ...) {
  # Look up the model's interface (e.g. formula, recipes, etc) 
  # and delagate to the connector functions (`recipe_to_formula` etc)
}

###################################################################

formula_to_recipe <- function(formula, data) {
  # execute the formula
  # extract terms _and roles_
  # put into recipe
  
}

formula_to_xy <- function(formula, data) {
  
}

###################################################################

recipe_to_formula <- function(recipe, data) {

}

recipe_to_xy <- function(recipe, data) {

}

###################################################################

xy_to_formula <- function(x, y) {
  
}

xy_to_recipe <- function(x, y) {
  
}




