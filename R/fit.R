# General TODOs
# Q: think about case weights in each instance below

# TODO write a better deparser for calls to avoid off-screen text and tabs

#' Fit a Model Specification to a Dataset
#'
#' `fit` and `fit_xy` take a model specification, translate the required
#'  code by substituting arguments, and execute the model fit
#'  routine.
#'
#' @param object An object of class `model_spec`
#' @param formula An object of class "formula" (or one that can
#'  be coerced to that class): a symbolic description of the model
#'  to be fitted.
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
#' @details  `fit` and `fit_xy` substitute the current arguments in the model
#'  specification into the computational engine's code, checks them
#'  for validity, then fits the model using the data and the
#'  engine-specific code. Different model functions have different
#'  interfaces (e.g. formula or `x`/`y`) and these functions translate
#'  between the interface used when `fit` or `fit_xy` were invoked and the one
#'  required by the underlying model.
#'
#' When possible, these functions attempt to avoid making copies of the
#'  data. For example, if the underlying model uses a formula and
#'  `fit` is invoked, the original data are references
#'  when the model is fit. However, if the underlying model uses
#'  something else, such as `x`/`y`, the formula is evaluated and
#'  the data are converted to the required format. In this case, any
#'  calls in the resulting model objects reference the temporary
#'  objects used to fit the model.
#' @examples
#' # Although `glm` only has a formula interface, different
#' # methods for specifying the model can be used
#'
#' library(dplyr)
#' data("lending_club")
#'
#' lm_mod <- logistic_reg()
#'
#' lm_mod <- logistic_reg()
#'
#' using_formula <-
#'   lm_mod %>%
#'   fit(Class ~ funded_amnt + int_rate,
#'       data = lending_club,
#'       engine = "glm")
#'
#' using_xy <-
#'   lm_mod %>%
#'   fit_xy(x = lending_club[, c("funded_amnt", "int_rate")],
#'          y = lending_club$Class,
#'          engine = "glm")
#'
#' using_formula
#' using_xy
#' @return A `model_fit` object that contains several elements:
#' \itemize{
#'   \item \code{lvl}: If the outcome is a factor, this contains
#'    the factor levels at the time of model fitting.
#'   \item \code{spec}: The model specification object
#'    (\code{object} in the call to \code{fit})
#'   \item \code{fit}: when the model is executed without error,
#'    this is the model object. Otherwise, it is a \code{try-error}
#'    object with the error message.
#'   \item \code{preproc}: any objects needed to convert between
#'    a formula and non-formula interface (such as the \code{terms}
#'    object)
#' }
#'  The return value will also have a class related to the fitted model (e.g.
#'  `"_glm"`) before the base class of `"model_fit"`.
#'
#' @param x A matrix or data frame of predictors.
#' @param y A vector, matrix or data frame of outcome data.
#' @rdname fit
#' @export
#' @export fit.model_spec
fit.model_spec <-
  function(object,
           formula = NULL,
           data = NULL,
           engine = object$engine,
           control = fit_control(),
           ...
  ) {
    dots <- quos(...)
    if (all(c("x", "y") %in% names(dots)))
      stop("`fit.model_spec` is for the formula methods. Use `fit_xy` instead.",
           call. = FALSE)
    cl <- match.call(expand.dots = TRUE)
    # Create an environment with the evaluated argument objects. This will be
    # used when a model call is made later.
    eval_env <- rlang::env()
    eval_env$data <- data
    eval_env$formula <- formula
    fit_interface <-
      check_interface(eval_env$formula, eval_env$data, cl, object)
    object$engine <- engine
    object <- check_engine(object)

    if (engine == "spark" && !inherits(eval_env$data, "tbl_spark"))
      stop(
        "spark objects can only be used with the formula interface to `fit` ",
        "with a spark data object.", call. = FALSE
      )

    # populate `method` with the details for this model type
    object <- get_method(object, engine = object$engine)

    check_installs(object)  # TODO rewrite with pkgman
    # TODO Should probably just load the namespace
    load_libs(object, control$verbosity < 2)

    interfaces <- paste(fit_interface, object$method$fit$interface, sep = "_")

    # Now call the wrappers that transition between the interface
    # called here ("fit" interface) that will direct traffic to
    # what the underlying model uses. For example, if a formula is
    # used here, `fit_interface_formula` will determine if a
    # translation has to be made if the model interface is x/y/
    res <-
      switch(
        interfaces,
        # homogeneous combinations:
        formula_formula =
          form_form(
            object = object,
            control = control,
            env = eval_env
          ),

        # heterogenous combinations
        formula_matrix =
          form_xy(
            object = object,
            control = control,
            env = eval_env,
            target = object$method$fit$interface,
            ...
          ),
        formula_data.frame =
          form_xy(
            object = object,
            control = control,
            env = eval_env,
            target = object$method$fit$interface,
            ...
          ),

        stop(interfaces, " is unknown")
      )
    model_classes <- class(res$fit)
    class(res) <- c(paste0("_", model_classes[1]), "model_fit")
    res
}

# ------------------------------------------------------------------------------

#' @rdname fit
#' @export
#' @inheritParams fit.model_spec
#'
fit_xy <- function(object, ...)
  UseMethod("fit_xy")

#' @rdname fit
#' @export
#' @export fit_xy.model_spec
#' @inheritParams fit.model_spec
fit_xy.model_spec <-
  function(object,
           x = NULL,
           y = NULL,
           engine = object$engine,
           control = fit_control(),
           ...
  ) {
    cl <- match.call(expand.dots = TRUE)
    eval_env <- rlang::env()
    eval_env$x <- x
    eval_env$y <- y
    fit_interface <-
      check_xy_interface(eval_env$x, eval_env$y, cl, object)
    object$engine <- engine
    object <- check_engine(object)

    if (engine == "spark")
      stop(
        "spark objects can only be used with the formula interface to `fit` ",
        "with a spark data object.", call. = FALSE
      )

    # populate `method` with the details for this model type
    object <- get_method(object, engine = object$engine)

    check_installs(object)  # TODO rewrite with pkgman
    # TODO Should probably just load the namespace
    load_libs(object, control$verbosity < 2)

    interfaces <- paste(fit_interface, object$method$fit$interface, sep = "_")

    # Now call the wrappers that transition between the interface
    # called here ("fit" interface) that will direct traffic to
    # what the underlying model uses. For example, if a formula is
    # used here, `fit_interface_formula` will determine if a
    # translation has to be made if the model interface is x/y/
    res <-
      switch(
        interfaces,
        # homogeneous combinations:
        matrix_matrix = , data.frame_matrix =
          xy_xy(
            object = object,
            env = eval_env,
            control = control,
            target = "matrix",
            ...
          ),

        data.frame_data.frame =, matrix_data.frame =
          xy_xy(
            object = object,
            env = eval_env,
            control = control,
            target = "data.frame",
            ...
          ),

        # heterogenous combinations
        matrix_formula =,  data.frame_formula =
          xy_form(
            object = object,
            env = eval_env,
            control = control,
            ...
          ),
        stop(interfaces, " is unknown")
      )
    model_classes <- class(res$fit)
    class(res) <- c(paste0("_", model_classes[1]), "model_fit")
    res
  }

# ------------------------------------------------------------------------------

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

# ------------------------------------------------------------------------------

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
        "`", obj, "` should be one of the following classes: ",
        paste0("'", cls, "'", collapse = ", "), call. = FALSE
      )
    else
      stop(
        "`", obj, "` should be a ", cls, " object", call. = FALSE
      )
  }
  invisible(x)
}

# ------------------------------------------------------------------------------


has_both_or_none <- function(a, b)
  (!is.null(a) & is.null(b)) | (is.null(a) & !is.null(b))

check_interface <- function(formula, data, cl, model) {
  inher(formula, "formula", cl)
  inher(data, c("data.frame", "tbl_spark"), cl)

  # Determine the `fit` interface
  form_interface <- !is.null(formula) & !is.null(data)

  if (form_interface)
    return("formula")
  stop("Error when checking the interface")
}

check_xy_interface <- function(x, y, cl, model) {
  inher(x, c("data.frame", "matrix"), cl)

  # `y` can be a vector (which is not a class), or a factor (which is not a vector)
  if (!is.null(y) && !is.vector(y))
    inher(y, c("data.frame", "matrix", "factor"), cl)

  # rule out spark data sets that don't use the formula interface
  if (inherits(x, "tbl_spark") | inherits(y, "tbl_spark"))
    stop("spark objects can only be used with the formula interface via `fit` ",
         "with a spark data object.", call. = FALSE)

  # Determine the `fit` interface
  matrix_interface <- !is.null(x) & !is.null(y) && is.matrix(x)
  df_interface <- !is.null(x) & !is.null(y) && is.data.frame(x)

  if (inherits(model, "surv_reg") &&
      (matrix_interface | df_interface))
    stop("Survival models must use the formula interface.", call. = FALSE)

  if (matrix_interface)
    return("data.frame")
  if (df_interface)
    return("data.frame")
  stop("Error when checking the interface")
}

#' @method print model_fit
#' @export
print.model_fit <- function(x, ...) {
  cat("parsnip model object\n\n")

  if(inherits(x$fit, "try-error")) {
    cat("Model fit failed with error:\n", x$fit, "\n")
  } else {
    print(x$fit, ...)
  }
  invisible(x)
}

