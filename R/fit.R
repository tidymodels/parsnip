# General TODOs
# Q: think about case weights in each instance below
# Q: where/how to add data checks (e.g. factors for classification)


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
#' @param .control A named list with elements `verbosity` and
#'  `catch`. `verbosity` should be an integer where a value of zero
#'  indicates that no messages or output should be shown when
#'  packages are loaded or when the model is fit. A value of 1 means
#'  that package loading is quiet but model fits can produce output
#'  to the screen (depending on if they contain their own
#'  `verbose`-type argument). A value of 2 or more indicates that
#'  any output should be seen. `catch` is a logical where a value of
#'  `TRUE` will evaluate the model inside of `try(, silent = TRUE)`.
#'  If the model fails, an object is still returned (without an
#'  error) that inherits the class "try-error".
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
#' @examples 
#' # Although `glm` only has a formula interface, different
#' # methods for specifying the model can be used
#'
#' data("lending_club")
#' 
#' lm_mod <- logistic_reg()
#' 
#' using_formula <- 
#'   fit(lm_mod, engine = "glm",
#'       Class ~ funded_amnt + int_rate,
#'       data = lending_club)
#' 
#' using_xy <- 
#'   fit(lm_mod, engine = "glm",
#'       x = lending_club[, c("funded_amnt", "int_rate")],
#'       y = lending_club$Class)
#' 
#' library(recipes)
#' lend_rec <- recipe(Class ~ funded_amnt + int_rate, 
#'                    data = lending_club)
#' 
#' using_recipe <- 
#'   fit(lm_mod, engine = "glm",
#'       lend_rec,
#'       data = lending_club)
#' 
#' coef(using_formula)
#' coef(using_xy)
#' coef(using_recipe)
#' 
#' # Using other options:
#' 
#' @export
#' @rdname fit 
fit <- function (object, ...) 
  UseMethod("fit")

# The S3 part here is awful (for now I hope)

#' @return An object for the fitted model. 
#' @export
#' @rdname fit 
fit.model_spec <- function(object, x, engine = object$engine, 
                           .control = list(verbosity = 1, catch = FALSE), 
                           ...) {
  object$engine <- engine
  object <- check_engine(object)
  .control <- check_control(.control)
  
  # sub in arguments to actual syntax for corresponding engine
  object <- finalize(object, engine = object$engine)
  check_installs(object)
  
  if (inherits(x, "formula")) {
    res <- fit_formula(object, formula = x, .control = .control, ...)
  } else {
    if (inherits(x, c("matrix", "data.frame"))) {
      res <- fit_xy(object, x = x, .control = .control, ...)
    } else {
      if (inherits(x, "recipe")) {
        res <- fit_recipe(object, recipe = x, .control = .control, ...)
      } else {
        stop("`x` should be a formula, data frame, matrix, or recipe")
      }
    }
  }
  res
}

###################################################################

#' @importFrom stats as.formula
fit_formula <- function(object, formula, engine = engine, .control, ...) {
  opts <- quos(...)
  
  if(!any(names(opts) == "data"))
    stop("Please pass a data frame with the `data` argument.",
         call. = FALSE)
  
  # TODO Should probably just load the namespace
  load_libs(object, .control$verbosity < 2)
  
  # Look up the model's interface (e.g. formula, recipes, etc) 
  # and delagate to the connector functions (`formula_to_recipe` etc)
  if(object$method$interface == "formula") {
    fit_expr <- object$method$fit_call
    fit_expr[["data"]] <- rlang::get_expr(opts$data)
    fit_expr$formula <- rlang::get_expr(formula)
    res <- eval_mod(fit_expr, capture = .control$verbosity == 0, catch = .control$catch)
  } else {
    if(object$method$interface %in% c("data.frame", "matrix")) {
      res <- formula_to_xy(object = object, formula = formula, data = opts["data"], .control)
    } else {
      stop("I don't know about the ", 
           object$method$interface, " interface.",
           call. = FALSE)
    }
  }
  res
}

fit_xy <- function(object, x, .control, ...) {
  opts <- quos(...)
  
  if(!any(names(opts) == "y"))
    stop("Please pass a data frame with the `y` argument.",
         call. = FALSE)
  
  # TODO Should probably just load the namespace
  load_libs(object, .control$verbosity < 2)
  
  # Look up the model's interface (e.g. formula, recipes, etc) 
  # and delegate to the connector functions (`xy_to_formula` etc)
  if(object$method$interface == "formula") {
    res <- xy_to_formula(object = object, x = x, y = opts["y"], .control)
  } else {
    if(object$method$interface %in% c("data.frame", "matrix")) {
      fit_expr <- object$method$fit_call
      
      if(is_missing_arg(fit_expr[["x"]]))
        fit_expr[["x"]] <- quote(x)
      if(is_missing_arg(fit_expr[["y"]]))
        fit_expr[["y"]] <- rlang::get_expr(opts$y) 

      res <- eval_mod(fit_expr, capture = .control$verbosity == 0, catch = .control$catch, env = get_env())
    } else {
      stop("I don't know about the ", 
           object$method$interface, " interface.",
           call. = FALSE)
    }
  }
  res  
}

fit_recipe <- function(object, recipe, .control, ...) {
  opts <- quos(...)
  
  if(!any(names(opts) == "data"))
    stop("Please pass a data frame with the `data` argument.",
         call. = FALSE)
  
  # TODO Should probably just load the namespace
  load_libs(object, .control$verbosity < 2)
  
  # Look up the model's interface (e.g. formula, recipes, etc) 
  # and delegate to the connector functions (`recipe_to_formula` etc)
  if(object$method$interface == "formula") {
    res <- recipe_to_formula(object = object, recipe = recipe, data = opts["data"], .control)
  } else {
    if(object$method$interface %in% c("data.frame", "matrix")) {
      res <- recipe_to_xy(object = object, recipe = recipe, data = opts["data"], .control)
    } else {
      stop("I don't know about the ", 
           object$method$interface, " interface.",
           call. = FALSE)
    }
  }
  res   
  
}

###################################################################

formula_to_recipe <- function(object, formula, data, .control) {
  # execute the formula
  # extract terms _and roles_
  # put into recipe
  
}

# TODO find a hook for whether to make dummies or not. Test cases
# are all numeric. Solve via the fit the method object via entry for
# `requires_dummies`

#' @importFrom  stats model.frame model.response terms
formula_to_xy <- function(object, formula, data, .control) {
  # Q: how do we fill in the other standard things here (subset, contrasts etc)?
  # Q: add a "matrix" option here and invoke model.matrix
  # Q: avoid eval using ?get_expr(data[["data"]])
  x <- stats::model.frame(formula, eval_tidy(data[["data"]]))
  y <- model.response(x)
  
  if(is_missing_arg(object$method$fit_call[["x"]]))
    object$method$fit_call[["x"]] <- quote(x)
  if(is_missing_arg(object$method$fit_call[["y"]]))
    object$method$fit_call[["y"]] <- quote(y)
  
  # Remove outcome column from `x`
  outcome_cols <- attr(terms(x), "response")
  if (!isTRUE(all.equal(outcome_cols, 0))) {
    x <- x[,-outcome_cols, drop = FALSE]
  }
  
  eval_mod(
    object$method$fit_call,
    capture = .control$verbosity == 0,
    catch = .control$catch,
    env = get_env()
  )
}

###################################################################

#' @importFrom recipes prep juice all_predictors all_outcomes
recipe_to_formula <- function(object, recipe, data, .control) {
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
  
  fit_expr <- object$method$fit_call
  fit_expr$formula <- as.formula(paste0(y_names, "~."))
  fit_expr$data <- quote(dat)
  eval_mod(
    fit_expr,
    capture = .control$verbosity == 0,
    catch = .control$catch,
    env = get_env()
  )
}

recipe_to_xy <- function(object, recipe, data, .control) {
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
  
  fit_expr <- object$method$fit_call

  if(is_missing_arg(fit_expr[["x"]]))
    fit_expr[["x"]] <- quote(x)
  if(is_missing_arg(fit_expr[["y"]]))
    fit_expr[["y"]] <- quote(y)
  
  eval_mod(
    fit_expr,
    capture = .control$verbosity == 0,
    catch = .control$catch,
    env = get_env()
  )
}

###################################################################

xy_to_formula <- function(object, x, y, .control) {
  if(!is.data.frame(x))
    x <- as.data.frame(x)
  x$.y <- eval_tidy(y[["y"]])
  fit_expr <- object$method$fit_call
  fit_expr$formula <- as.formula(.y ~ .)
  fit_expr$data <- quote(x)
  eval_tidy(fit_expr)
}

xy_to_recipe <- function(object, x, y, .control) {
  
}

###################################################################

#' @importFrom utils capture.output
eval_mod <- function(e, capture = FALSE, catch = FALSE, ...) {
  if (capture) {
    if (catch) {
      junk <- capture.output(res <- try(eval_tidy(e, ...), silent = TRUE))
    } else {
      junk <- capture.output(res <- eval_tidy(e, ...))
    }
  } else {
    if (catch) {
      res <- try(eval_tidy(e, ...), silent = TRUE)
    } else {
      res <- eval_tidy(e, ...)
    }
  }
  res
}

###################################################################


check_control <- function(x) {
  if (!is.list(x))
    stop(".control should be a named list.", call. = FALSE)
  if (!isTRUE(all.equal(sort(names(x)), c("catch", "verbosity"))))
    stop(".control should be a named list with elements 'verbosity' and 'catch'.",
         call. = FALSE)
  # based on ?is.integer
  int_check <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  if (!int_check(x$verbosity))
    stop("verbosity should be an integer.", call. = FALSE)
  if (!is.logical(x$catch))
    stop("catch should be a logical.", call. = FALSE)
  x
}
