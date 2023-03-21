#' Fit a Model Specification to a Dataset
#'
#' `fit()` and `fit_xy()` take a model specification, translate the required
#'  code by substituting arguments, and execute the model fit
#'  routine.
#'
#' @param object An object of class `model_spec` that has a chosen engine
#'  (via [set_engine()]).
#' @param formula An object of class `formula` (or one that can
#'  be coerced to that class): a symbolic description of the model
#'  to be fitted.
#' @param data Optional, depending on the interface (see Details
#'  below). A data frame containing all relevant variables (e.g.
#'  outcome(s), predictors, case weights, etc). Note: when needed, a
#'  \emph{named argument} should be used.
#' @param case_weights An optional classed vector of numeric case weights. This
#'   must return `TRUE` when [hardhat::is_case_weights()] is run on it. See
#'   [hardhat::frequency_weights()] and [hardhat::importance_weights()] for
#'   examples.
#' @param control A named list with elements `verbosity` and
#'  `catch`. See [control_parsnip()].
#' @param ... Not currently used; values passed here will be
#'  ignored. Other options required to fit the model should be
#'  passed using `set_engine()`.
#' @details  `fit()` and `fit_xy()` substitute the current arguments in the model
#'  specification into the computational engine's code, check them
#'  for validity, then fit the model using the data and the
#'  engine-specific code. Different model functions have different
#'  interfaces (e.g. formula or `x`/`y`) and these functions translate
#'  between the interface used when `fit()` or `fit_xy()` was invoked and the one
#'  required by the underlying model.
#'
#' When possible, these functions attempt to avoid making copies of the
#'  data. For example, if the underlying model uses a formula and
#'  `fit()` is invoked, the original data are references
#'  when the model is fit. However, if the underlying model uses
#'  something else, such as `x`/`y`, the formula is evaluated and
#'  the data are converted to the required format. In this case, any
#'  calls in the resulting model objects reference the temporary
#'  objects used to fit the model.
#'
#' If the model engine has not been set, the model's default engine will be used
#'  (as discussed on each model page). If the `verbosity` option of
#'  [control_parsnip()] is greater than zero, a warning will be produced.
#'
#' If you would like to use an alternative method for generating contrasts when
#' supplying a formula to `fit()`, set the global option `contrasts` to your
#' preferred method. For example, you might set it to:
#' `options(contrasts = c(unordered = "contr.helmert", ordered = "contr.poly"))`.
#' See the help page for [stats::contr.treatment()] for more possible contrast
#' types.
#'
#' For models with `"censored regression"` modes, an additional computation is
#' executed and saved in the parsnip object. The `censor_probs` element contains
#' a "reverse Kaplan-Meier" curve that models the probability of censoring. This
#' may be used later to compute inverse probability censoring weights for
#' performance measures.
#' @examples
#' # Although `glm()` only has a formula interface, different
#' # methods for specifying the model can be used
#'
#' library(dplyr)
#' library(modeldata)
#' data("lending_club")
#'
#' lr_mod <- logistic_reg()
#'
#' using_formula <-
#'   lr_mod %>%
#'   set_engine("glm") %>%
#'   fit(Class ~ funded_amnt + int_rate, data = lending_club)
#'
#' using_xy <-
#'   lr_mod %>%
#'    set_engine("glm") %>%
#'   fit_xy(x = lending_club[, c("funded_amnt", "int_rate")],
#'          y = lending_club$Class)
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
#' @seealso [set_engine()], [control_parsnip()], `model_spec`, `model_fit`
#' @param x A matrix, sparse matrix, or data frame of predictors. Only some
#' models have support for sparse matrix input. See `parsnip::get_encoding()`
#' for details. `x` should have column names.
#' @param y A vector, matrix or data frame of outcome data.
#' @rdname fit
#' @export
#' @export fit.model_spec
fit.model_spec <-
  function(object,
           formula,
           data,
           case_weights = NULL,
           control = control_parsnip(),
           ...
  ) {
    if (object$mode == "unknown") {
      rlang::abort("Please set the mode in the model specification.")
    }
    control <- condense_control(control, control_parsnip())
    check_case_weights(case_weights, object)

    dots <- quos(...)

    if (length(possible_engines(object)) == 0) {
      prompt_missing_implementation(
        spec = object,
        prompt = cli::cli_abort,
        call = call2("fit")
      )
    }
    if (is.null(object$engine)) {
      eng_vals <- possible_engines(object)
      object$engine <- eng_vals[1]
      if (control$verbosity > 0) {
        rlang::warn(glue::glue("Engine set to `{object$engine}`."))
      }
    }

    if (all(c("x", "y") %in% names(dots))) {
      rlang::abort("`fit.model_spec()` is for the formula methods. Use `fit_xy()` instead.")
    }
    cl <- match.call(expand.dots = TRUE)
    # Create an environment with the evaluated argument objects. This will be
    # used when a model call is made later.
    eval_env <- rlang::env()

    wts <- weights_to_numeric(case_weights, object)

    formula <- patch_formula_environment_with_case_weights(
      formula = formula,
      data = data,
      case_weights = wts
    )

    eval_env$data <- data
    eval_env$formula <- formula
    eval_env$weights <- wts

    fit_interface <-
      check_interface(eval_env$formula, eval_env$data, cl, object)

    if (object$engine == "spark" && !inherits(eval_env$data, "tbl_spark"))
      rlang::abort(
        glue::glue(
          "spark objects can only be used with the formula interface to `fit()` ",
          "with a spark data object."
        )
      )

    # populate `method` with the details for this model type
    object <- add_methods(object, engine = object$engine)

    check_installs(object)

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

        rlang::abort(glue::glue("{interfaces} is unknown."))
      )
    res$censor_probs <- reverse_km(object, eval_env)
    model_classes <- class(res$fit)
    class(res) <- c(paste0("_", model_classes[1]), "model_fit")
    res
}

# ------------------------------------------------------------------------------

#' @rdname fit
#' @export
#' @export fit_xy.model_spec
fit_xy.model_spec <-
  function(object,
           x,
           y,
           case_weights = NULL,
           control = control_parsnip(),
           ...
  ) {
    if (object$mode == "unknown") {
      rlang::abort("Please set the mode in the model specification.")
    }

    if (inherits(object, "surv_reg")) {
      rlang::abort("Survival models must use the formula interface.")
    }

    control <- condense_control(control, control_parsnip())

    if (is.null(colnames(x))) {
      rlang::abort("'x' should have column names.")
    }
    check_case_weights(case_weights, object)

    dots <- quos(...)
    if (is.null(object$engine)) {
      eng_vals <- possible_engines(object)
      object$engine <- eng_vals[1]
      if (control$verbosity > 0) {
        rlang::warn(glue::glue("Engine set to `{object$engine}`."))
      }
    }

    if (object$engine != "spark" & NCOL(y) == 1 & !(is.vector(y) | is.factor(y))) {
      if (is.matrix(y)) {
        y <- y[, 1]
      } else {
        y <- y[[1]]
      }
    }

    cl <- match.call(expand.dots = TRUE)
    eval_env <- rlang::env()
    eval_env$x <- x
    eval_env$y <- y

    if(object$engine == "xgboost" && !is.null(case_weights)){
      # Pass as raw to preserve weight type e.g. frequency, importance
      eval_env$weights <- case_weights
    } else {
      eval_env$weights <- weights_to_numeric(case_weights, object)
    }

    # TODO case weights: pass in eval_env not individual elements
    fit_interface <- check_xy_interface(eval_env$x, eval_env$y, cl, object)

    if (object$engine == "spark")
      rlang::abort(
        glue::glue(
          "spark objects can only be used with the formula interface to `fit()` ",
          "with a spark data object."
        )
      )

    # populate `method` with the details for this model type
    object <- add_methods(object, engine = object$engine)

    check_installs(object)

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

        data.frame_data.frame = , matrix_data.frame =
          xy_xy(
            object = object,
            env = eval_env,
            control = control,
            target = "data.frame",
            ...
          ),

        # heterogenous combinations
        matrix_formula = ,  data.frame_formula =
          xy_form(
            object = object,
            env = eval_env,
            control = control,
            ...
          ),
        rlang::abort(glue::glue("{interfaces} is unknown."))
      )
    res$censor_probs <- reverse_km(object, eval_env)
    model_classes <- class(res$fit)
    class(res) <- c(paste0("_", model_classes[1]), "model_fit")
    res
  }

# ------------------------------------------------------------------------------

eval_mod <- function(e, capture = FALSE, catch = FALSE, envir = NULL, ...) {
  if (capture) {
    if (catch) {
      junk <- capture.output(res <- try(eval_tidy(e, env = envir, ...), silent = TRUE))
    } else {
      junk <- capture.output(res <- eval_tidy(e, env = envir, ...))
    }
  } else {
    if (catch) {
      res <- try(eval_tidy(e, env = envir, ...), silent = TRUE)
    } else {
      res <- eval_tidy(e, env = envir, ...)
    }
  }
  res
}

# ------------------------------------------------------------------------------

inher <- function(x, cls, cl) {
  if (!is.null(x) && !inherits(x, cls)) {
    call <- match.call()
    obj <- deparse(call[["x"]])
    if (length(cls) > 1)
      rlang::abort(
        glue::glue(
          "`{obj}` should be one of the following classes: ",
          glue::glue_collapse(glue::glue("'{cls}'"), sep = ", ")
        )
      )
    else
      rlang::abort(
        glue::glue("`{obj}` should be a {cls} object")
      )
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

check_interface <- function(formula, data, cl, model) {
  inher(formula, "formula", cl)
  inher(data, c("data.frame", "tbl_spark"), cl)

  # Determine the `fit()` interface
  form_interface <- !is.null(formula) & !is.null(data)

  if (form_interface)
    return("formula")
  rlang::abort("Error when checking the interface.")
}

check_xy_interface <- function(x, y, cl, model) {

  sparse_ok <- allow_sparse(model)
  sparse_x <- inherits(x, "dgCMatrix")
  if (!sparse_ok & sparse_x) {
    rlang::abort("Sparse matrices not supported by this model/engine combination.")
  }

  if (sparse_ok) {
    inher(x, c("data.frame", "matrix", "dgCMatrix"), cl)
  } else {
    inher(x, c("data.frame", "matrix"), cl)
  }

  # `y` can be a vector (which is not a class), or a factor or
  # Surv object (which are not vectors)
  if (!is.null(y) && !is.vector(y))
    inher(y, c("data.frame", "matrix", "factor", "Surv"), cl)

  # rule out spark data sets that don't use the formula interface
  if (inherits(x, "tbl_spark") | inherits(y, "tbl_spark"))
    rlang::abort(
      glue::glue(
        "spark objects can only be used with the formula interface via `fit()` ",
        "with a spark data object."
        )
      )


  if (sparse_ok) {
    matrix_interface <- !is.null(x) && !is.null(y) && (is.matrix(x) | sparse_x)
  } else {
    matrix_interface <- !is.null(x) && !is.null(y) && is.matrix(x)
  }

  df_interface <- !is.null(x) & !is.null(y) && is.data.frame(x)

  if (matrix_interface) {
    return("matrix")
  }
  if (df_interface) {
    return("data.frame")
  }
  rlang::abort("Error when checking the interface")
}

allow_sparse <- function(x) {
  res <- get_from_env(paste0(class(x)[1], "_encoding"))
  all(res$allow_sparse_x[res$engine == x$engine])
}

#' @method print model_fit
#' @export
print.model_fit <- function(x, ...) {
  cat("parsnip model object\n\n")
  if (!is.na(x$elapsed[["elapsed"]])) {
    cat("Fit time: ", prettyunits::pretty_sec(x$elapsed[["elapsed"]]), "\n")
  }

  if (inherits(x$fit, "try-error")) {
    cat("Model fit failed with error:\n", x$fit, "\n")
  } else {
    print(x$fit, ...)
  }
  invisible(x)
}
