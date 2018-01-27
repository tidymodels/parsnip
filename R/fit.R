# General TODOs
# - think about case weights in each instance below
# - try/catch all model fit evaluations
# - option to capture output/verboseness
# - devise a unit test plan that does not add pkg deps for each model


#' Fit a Model Specification to a Dataset
#' 
#' `fit` will take a model specification, finalize the required
#'  code by substituting arguments, and execute the model fit
#'  routine.
#' 
#' @param object An object of class `model_spec`
#' @param x Either an R formula, a data frame of predictors, or a
#'  recipe object.
#' @param engine A character string for the software that should
#'  be used to fit the model. This is highly dependent on the type
#'  of model (e.g. linear regression, random forest, etc.).
#' @param ... Other options required to fit the model. If `x` is a
#'  formula or recipe, then the `data` argument should be passed
#'  here. For the "x/y" interface, the outcome data should be passed
#'  in with the argument `y`.
#' @details  `fit` substitutes the current arguments in the model
#'  specification into the computational engine's code, checks them
#'  for validity, then fits the model using the data and the
#'  engine-specific code. Different model functions have different
#'  interfaces (e.g. formula or `x`/`y`) and `fit` translates
#'  between the interface used when `fit` was invoked and the one
#'  required by the underlying model.
#' 
#' When possible, `fit` attempts to avoid making copies of the
#'  data. For example, if the underlying model uses a formula and
#'  fit is invoked with a formula, the original data are references
#'  when the model is fit. However, if the underlying model uses
#'  something else, such as `x`/`y`, the formula is evaluated and
#'  the data are converted to the required format. In this case, any
#'  calls in the resulting model objects reference the temporary
#'  objects used to fit the model.
#' @export
#' @rdname fit 
fit <- function (object, ...) 
  UseMethod("fit")

# The S3 part here is awful (for now I hope)

#' @return An object for the fitted model. 
#' @export
#' @rdname fit 
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
    res <- eval_tidy(fit_expr)
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
  opts <- quos(...)
  
  if(!any(names(opts) == "y"))
    stop("Please pass a data frame with the `y` argument.",
         call. = FALSE)
  
  # TODO Should probably just load the namespace
  for(pkg in object$method$library)
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  
  # Look up the model's interface (e.g. formula, recipes, etc) 
  # and delegate to the connector functions (`xy_to_formula` etc)
  if(object$method$interface == "formula") {
    res <- xy_to_formula(object = object, x = x, y = opts["y"])
  } else {
    if(object$method$interface %in% c("data.frame", "matrix")) {
      fit_expr <- sub_arg_values(object$method$fit, opts["y"])
      res <- eval_tidy(fit_expr)
    } else {
      stop("I don't know about the ", 
           object$method$interface, " interface.",
           call. = FALSE)
    }
  }
  res  
}

fit_recipe <- function(object, recipe, ...) {
  opts <- quos(...)
  
  if(!any(names(opts) == "data"))
    stop("Please pass a data frame with the `data` argument.",
         call. = FALSE)
  
  # TODO Should probably just load the namespace
  for(pkg in object$method$library)
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  
  # Look up the model's interface (e.g. formula, recipes, etc) 
  # and delegate to the connector functions (`recipe_to_formula` etc)
  if(object$method$interface == "formula") {
    res <- recipe_to_formula(object = object, recipe = recipe, data = opts["data"])
  } else {
    if(object$method$interface %in% c("data.frame", "matrix")) {
      res <- recipe_to_xy(object = object, recipe = recipe, data = opts["data"])
    } else {
      stop("I don't know about the ", 
           object$method$interface, " interface.",
           call. = FALSE)
    }
  }
  res   
  
}

###################################################################

formula_to_recipe <- function(object, formula, data) {
  # execute the formula
  # extract terms _and roles_
  # put into recipe
  
}

#' @importFrom  stats model.frame model.response
formula_to_xy <- function(object, formula, data) {
  # TODO how do we fill in the other standard things here (subset, contrasts etc)?
  # TODO add a "matrix" option here and invoke model.matrix
  
  # Q: avoid eval using ?get_expr(data[["data"]])
  x <- stats::model.frame(formula, eval_tidy(data[["data"]]))
  y <- model.response(x, "numeric")
  eval_tidy(object$method$fit)
}

###################################################################

#' @importFrom recipes prep juice all_predictors all_outcomes
recipe_to_formula <- function(object, recipe, data) {
  # TODO case weights
  recipe <-
    prep(recipe, training = eval_tidy(data[["data"]]), retain = TRUE)
  dat <- juice(recipe, all_predictors(), all_outcomes())
  dat <- as.data.frame(dat)
  
  data_info <- summary(recipe)
  y_names <- data_info$variable[data_info$role == "outcome"]
  if (length(y_names) > 1)
    y_names <-
    paste0("cbind(", paste0(y_names, collapse = ","), ")")
  
  fit_expr <- object$method$fit
  fit_expr$formula <- as.formula(paste0(y_names, "~."))
  fit_expr$data <- quote(dat)
  eval_tidy(fit_expr)
}

recipe_to_xy <- function(object, recipe, data) {
  # TODO case weights
  recipe <-
    prep(recipe, training = eval_tidy(data[["data"]]), retain = TRUE)
  
  x <- juice(recipe, all_predictors())
  x <- as.data.frame(x)
  y <- juice(recipe, all_outcomes())
  if (ncol(y) > 1)
    y <- as.data.frame(y)
  else
    y <- y[[1]]
  
  fit_expr <- object$method$fit
  fit_expr$x <- quote(x)
  fit_expr$y <- quote(y)
  eval_tidy(fit_expr)
}

###################################################################

xy_to_formula <- function(object, x, y) {
  if(!is.data.frame(x))
    x <- as.data.frame(x)
  x$.y <- eval_tidy(y[["y"]])
  fit_expr <- object$method$fit
  fit_expr$formula <- as.formula(.y ~ .)
  fit_expr$data <- quote(x)
  eval_tidy(fit_expr)
}

xy_to_recipe <- function(object, x, y) {
  
}
