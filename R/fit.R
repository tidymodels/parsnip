# General TODOs
# Q: think about case weights in each instance below
# Q: where/how to add data checks (e.g. factors for classification)


#' Fit a Model Specification to a Dataset
#'
#' `fit` will take a model specification, translate the required
#'  code by substituting arguments, and execute the model fit
#'  routine.
#'
#' @param object An object of class `model_spec`
#' @param formula An object of class "formula" (or one that can
#'  be coerced to that class): a symbolic description of the model
#'  to be fitted.
#' @param recipe Optional, depending on the interface (see Details
#'  below). An object of class [recipes::recipe()]. Note: when
#'  needed, a \emph{named argument} should be used.
#' @param x Optional, depending on the interface (see Details
#'  below). Can be data frame or matrix of predictors. Note: when
#'  needed, a \emph{named argument} should be used.
#' @param y Optional, depending on the interface (see Details
#'  below). Can be a vector, data frame or matrix of predictors (the
#'  latter two in case of multivariate outcomes). Note: when needed,
#'  a \emph{named argument} should be used.
#' @param data Optional, depending on the interface (see Details
#'  below). A data frame containing all relevant variables (e.g.
#'  outcome(s), predictors, case weights, etc). Note: when needed, a
#'  \emph{named argument} should be used.
#' @param engine A character string for the software that should
#'  be used to fit the model. This is highly dependent on the type
#'  of model (e.g. linear regression, random forest, etc.).
#' @param control A named list with elements `verbosity` and
#'  `catch`. See [fit_control()].
#' @param ... Not currently used; values passed here will be
#'  ignored. Other options required to fit the model should be
#'  passed using the `others` argument in the original model
#'  specification.
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
#'   fit(lm_mod,
#'       Class ~ funded_amnt + int_rate,
#'       data = lending_club,
#'       engine = "glm")
#'
#' # NOTE: use named arguments for "x" and "y" when using this interface
#' using_xy <-
#'   fit(lm_mod,
#'       x = lending_club[, c("funded_amnt", "int_rate")],
#'       y = lending_club$Class,
#'       engine = "glm")
#'
#' # NOTE: use named arguments for "recipe" and "data" when using this interface
#' library(recipes)
#' lend_rec <- recipe(Class ~ funded_amnt + int_rate,
#'                    data = lending_club)
#'
#' using_recipe <-
#'   fit(lm_mod,
#'       recipe = lend_rec,
#'       data = lending_club,
#'       engine = "glm")
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

#' @return An object for the fitted model.
#' @export
#' @rdname fit
fit.model_spec <-
  function(object,
           formula = NULL,
           recipe = NULL,
           x = NULL,
           y = NULL,
           data = NULL,
           engine = object$engine,
           control = fit_control(),
           ...
  ) {
    call_interface <-
      check_interface(formula, recipe, x, y, data, match.call(expand.dots = TRUE))
    object$engine <- engine
    object <- check_engine(object)


    # sub in arguments to actual syntax for corresponding engine
    object <- translate(object, engine = object$engine)
    check_installs(object)  # TODO rewrite with pkgman
    # TODO Should probably just load the namespace
    load_libs(object, control$verbosity < 2)

    res <- switch(
      call_interface,
      formula = fit_formula(object, formula, data, control = control, ...),
      recipe = fit_recipe(object, recipe, data, control = control, ...),
      xy = fit_xy(object, x, y, control = control, ...),
      stop("Wrong interface type")
    )

    res
}

###################################################################

#' @importFrom stats as.formula
fit_formula <- function(object, formula, data, engine = engine, control, ...) {
  opts <- quos(...)
  # Look up the model's interface (e.g. formula, recipes, etc)
  # and delagate to the connector functions (`formula_to_recipe` etc)
  if(object$method$interface == "formula") {
    fit_expr <- object$method$fit_call
    fit_expr[["data"]] <- quote(data)
    fit_expr$formula <- rlang::get_expr(formula)
    res <-
      eval_mod(
        fit_expr,
        capture = control$verbosity == 0,
        catch = control$catch,
        env = current_env()
      )
  } else {
    if(object$method$interface %in% c("data.frame", "matrix", "spark")) {
      res <- formula_to_xy(object = object, formula = formula, data = data, control)
    } else {
      stop("I don't know about the ",
           object$method$interface, " interface.",
           call. = FALSE)
    }
  }
  res
}


xy_to_xy <- function(object, x, y, control, ...) {
  fit_expr <- object$method$fit_call
  fit_expr[["x"]] <- quote(x)
  fit_expr[["y"]] <- quote(y)
    eval_mod(
      fit_expr,
      capture = control$verbosity == 0,
      catch = control$catch,
      env = current_env()
    )
}
xy_to_matrix <- function(object, x, y, control, ...) {
  if (object$method$interface == "matrix" && !is.matrix(x))
    x <- as.matrix(x)
  xy_to_xy(object, x, y, control, ...)
}
xy_to_df <- function(object, x, y, control, ...) {
  if (object$method$interface == "data.frame" && !is.data.frame(x))
    x <- as.data.frame(x)
  xy_to_xy(object, x, y, control, ...)
}
xy_to_spark <- function(object, x, y, control, ...) {
  sdf <- sparklyr::sdf_bind_cols(x, y)
  fit_expr <- object$method$fit_call
  fit_expr[["x"]] <- quote(sdf)
  fit_expr[["features_col"]] <- quote(colnames(x))
  fit_expr[["label_col"]] <- quote(colnames(y))
    eval_mod(
      fit_expr,
      capture = control$verbosity == 0,
      catch = control$catch,
      env = current_env()
    )
}


fit_xy <- function(object, x, y, control, ...) {
  opts <- quos(...)

  res <- switch(
    object$method$interface,
    formula = xy_to_formula(object = object, x = x, y = y, control, ...),
    matrix = xy_to_matrix(object = object, x = x, y = y, control, ...),
    data.frame = xy_to_df(object = object, x = x, y = y, control, ...),
    spark = xy_to_spark(object = object, x = x, y = y, control, ...),
    stop("Unknown interface")
  )
  res
}

fit_recipe <- function(object, recipe, data, control, ...) {
  opts <- quos(...)

  # Look up the model's interface (e.g. formula, recipes, etc)
  # and delegate to the connector functions (`recipe_to_formula` etc)
  if(object$method$interface == "formula") {
    res <- recipe_to_formula(object = object, recipe = recipe, data = data, control)
  } else {
    if(object$method$interface %in% c("data.frame", "matrix")) {
      res <- recipe_to_xy(object = object, recipe = recipe, data = data, control)
    } else {
      stop("I don't know about the ",
           object$method$interface, " interface.",
           call. = FALSE)
    }
  }
  res

}


#placeholder
fit_spark <- function(object, remote, engine = engine, control, ...) {
  NULL
}

### or.... let `data, `x` and `y` be remote spark data tables or specifications



###################################################################

formula_to_recipe <- function(object, formula, data, control) {
  # execute the formula
  # extract terms _and roles_
  # put into recipe

}

# TODO find a hook for whether to make dummies or not. Test cases
# are all numeric. Solve via the fit the method object via entry for
# `requires_dummies`

#' @importFrom  stats model.frame model.response terms
formula_to_xy <- function(object, formula, data, control) {
  # Q: how do we fill in the other standard things here (subset, contrasts etc)?
  # Q: add a "matrix" option here and invoke model.matrix
  x <- stats::model.frame(formula, data)
  y <- model.response(x)

  # Remove outcome column(s) from `x`
  outcome_cols <- attr(terms(x), "response")
  if (!isTRUE(all.equal(outcome_cols, 0))) {
    x <- x[,-outcome_cols, drop = FALSE]
  }

  object$method$fit_call[["x"]] <- quote(x)
  object$method$fit_call[["y"]] <- quote(y)

  eval_mod(
    object$method$fit_call,
    capture = control$verbosity == 0,
    catch = control$catch,
    env = current_env()
  )
}

###################################################################

#' @importFrom recipes prep juice all_predictors all_outcomes
recipe_to_formula <- function(object, recipe, data, control) {
  # TODO case weights
  recipe <-
    prep(recipe, training = data, retain = TRUE, verbose = control$verbosity > 1)
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
    capture = control$verbosity == 0,
    catch = control$catch,
    env = current_env()
  )
}

recipe_to_xy <- function(object, recipe, data, control) {
  # TODO case weights
  recipe <-
    prep(recipe, training = data, retain = TRUE, verbose = control$verbosity > 1)

  x <- juice(recipe, all_predictors())
  x <- as.data.frame(x)
  y <- juice(recipe, all_outcomes())
  if (ncol(y) > 1)
    y <- as.data.frame(y)
  else
    y <- y[[1]]

  fit_expr <- object$method$fit_call

  fit_expr[["x"]] <- quote(x)
  fit_expr[["y"]] <- quote(y)

  eval_mod(
    fit_expr,
    capture = control$verbosity == 0,
    catch = control$catch,
    env = current_env()
  )
}

###################################################################

xy_to_formula <- function(object, x, y, control) {
  if(!is.data.frame(x))
    x <- as.data.frame(x)
  x$.y <- y
  fit_expr <- object$method$fit_call
  fit_expr$formula <- as.formula(.y ~ .)
  fit_expr$data <- quote(x)
  eval_tidy(fit_expr, env = current_env())
}

xy_to_recipe <- function(object, x, y, control) {

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
    stop("control should be a named list.", call. = FALSE)
  if (!isTRUE(all.equal(sort(names(x)), c("catch", "verbosity"))))
    stop("control should be a named list with elements 'verbosity' and 'catch'.",
         call. = FALSE)
  # based on ?is.integer
  int_check <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  if (!int_check(x$verbosity))
    stop("verbosity should be an integer.", call. = FALSE)
  if (!is.logical(x$catch))
    stop("catch should be a logical.", call. = FALSE)
  x
}

inher <- function(x, cls, cl) {
  if (!is.null(x) && !inherits(x, cls)) {
    call <- match.call()
    obj <- deparse(call[["x"]])
    if (length(cls) > 1)
      stop(
        show_call(cl),
        "`", obj, "` should be one of the following classes: ",
        paste0("'", cls, "'", collapse = ", "), call. = FALSE
      )
    else
      stop(
        show_call(cl),
        "`", obj, "` should be a ", cls, " object", call. = FALSE
      )
  }
  invisible(x)
}

###################################################################

show_call <- function(x)
  paste("`fit` call:\n ", paste(deparse(x), sep = "\n", collapse = "\n"),
        "\n", sep = "")

has_both_or_none <- function(a, b)
  (!is.null(a) & is.null(b)) | (is.null(a) & !is.null(b))

check_interface <- function(formula, recipe, x, y, data, cl) {
  inher(formula, "formula", cl)
  inher(recipe, "recipe", cl)
  inher(x, c("data.frame", "matrix", "tbl_spark"), cl)
  # `y` can be a vector (which is not a class), or a factor (which is not a vector)
  if(!is.null(y) && !is.vector(y))
    inher(y, c("data.frame", "matrix", "factor", "tbl_spark"), cl)
  inher(data, c("data.frame", "matrix", "tbl_spark"), cl)

  x_interface <- !is.null(x) & !is.null(y)
  rec_interface <- !is.null(recipe) & !is.null(data)
  form_interface <- !is.null(formula) & !is.null(data)
  if (!(x_interface | rec_interface | form_interface))
    stop("Incomplete specification of arguments; used either 'x/y', ",
         "'formula/data', or 'recipe/data' combinations.", call. = FALSE)
  if (sum(c(x_interface, rec_interface, form_interface)) > 1)
    stop("Too many specifications of arguments; used either 'x/y', ",
         "'formula/data', or 'recipe/data' combinations.", call. = FALSE)
  if(x_interface) return("xy")
  if(rec_interface) return("recipe")
  if(form_interface) return("formula")
  stop("Error in checking the interface")
}




