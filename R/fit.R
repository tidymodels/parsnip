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
    cl <- match.call(expand.dots = TRUE)
    fit_interface <-
      check_interface(formula, recipe, x, y, data, cl)
    object$engine <- engine
    object <- check_engine(object)

    if (engine == "spark" && !inherits(data, "tbl_spark"))
      stop(
        "spark objects can only be used with the formula interface to `fit` ",
        "with a spark data object.", call. = FALSE
      )

    # sub in arguments to actual syntax for corresponding engine
    object <- translate(object, engine = object$engine)
    check_installs(object)  # TODO rewrite with pkgman
    # TODO Should probably just load the namespace
    load_libs(object, control$verbosity < 2)

    res <- switch(
      fit_interface,
      formula = fit_interface_formula(
        object = object,
        formula = cl$formula,
        data = cl$data,
        control = control,
        ...
      ),
      matrix = fit_interface_matrix(x, y, object, control, ...),
      data.frame = fit_interface_data.frame(x, y, object, control, ...),
      recipe = fit_interface_recipe(recipe, data, object, control, ...),
      stop("Wrong interface type")
    )

    res
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
  if (!is.null(y) && !is.vector(y))
    inher(y, c("data.frame", "matrix", "factor", "tbl_spark"), cl)
  inher(data, c("data.frame", "matrix", "tbl_spark"), cl)

  # rule out spark data sets that don't use the formula interface
  if (inherits(x, "tbl_spark") | inherits(y, "tbl_spark"))
    stop("spark objects can only be used with the formula interface to `fit` ",
         "with a spark data object.", call. = FALSE)

  # Determine the `fit` interface
  matrix_interface <- !is.null(x) & !is.null(y) && is.matrix(x)
  df_interface <- !is.null(x) & !is.null(y) && is.data.frame(x)
  rec_interface <- !is.null(recipe) & !is.null(data)
  form_interface <- !is.null(formula) & !is.null(data)

  if (!(matrix_interface | df_interface | rec_interface | form_interface))
    stop("Incomplete specification of arguments; used either 'x/y', ",
         "'formula/data', or 'recipe/data' combinations.", call. = FALSE)
  if (sum(c(matrix_interface, df_interface, rec_interface, form_interface)) > 1)
    stop("Too many specifications of arguments; used either 'x/y', ",
         "'formula/data', or 'recipe/data' combinations.", call. = FALSE)

  if (matrix_interface)
    return("data.frame")
  if (df_interface)
    return("data.frame")
  if (rec_interface)
    return("recipe")
  if (form_interface)
    return("formula")
  stop("Error when checking the interface")
}




