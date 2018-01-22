

fit <- function (object, ...) 
  UseMethod("fit")

# The S3 part here is awful for now

fit_formula <- function(object, formula, data, verboseness = 0, engine = "ranger") {
  varying_param_check(object)
  
  # go between input methods =[
  
  # data checks based on method
  
  object <- finalize(object, engine = engine)
  if(verboseness == 0) {
    fit_obj <- eval(object$method$fit)
  } else {
    capture.output(fit_obj <- eval(object$method$fit))
  }
  fit_obj
}


fit.model_spec <- function(object, x, ...) {
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

f2r <- function(formula, data) {
  # execute the formula
  # extract terms _and roles_
  # put into recipe
  
}



r2f <- function(recipe?, data) {
  # execute the formula
  # extract terms _and roles_
  # put into recipe
  
}
