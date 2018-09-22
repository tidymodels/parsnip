#' General Interface for Parametric Survival Models
#'
#' `surv_reg` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  R. The main argument for the
#'  model is:
#' \itemize{
#'   \item \code{dist}: The probability distribution of the outcome.
#' }
#' This argument is converted to its specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using the `others` argument. If left to its default
#'  here (`NULL`), the value is taken from the underlying model
#'  functions.
#'
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `surv_reg`,the
#'  mode will always be "regression".
#'
#'  Since survival models typically involve censoring (and require the use of
#'  [survival::Surv()] objects), the [fit()] function will require that the
#'  survival model be specified via the formula interface.
#'  
#' Also, for the `flexsurv::flexsurvfit` engine, the typical
#'  `strata` function cannot be used. To achieve the same effect,
#'  the extra parameter roles can be used (as described above).
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"flexsurv"`
#' }
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param others A named list of arguments to be used by the
#'  underlying models (e.g., `flexsurv::flexsurvreg`). These are not evaluated
#'  until the model is fit and will be substituted into the model
#'  fit expression.
#' @param dist A character string for the outcome distribution. "weibull" is
#'  the default.
#' @param ... Used for S3 method consistency. Any arguments passed to
#'  the ellipses will result in an error. Use `others` instead.
#' @seealso [varying()], [fit()], [survival::Surv()]
#' @references Jackson, C. (2016). `flexsurv`: A Platform for Parametric Survival
#'  Modeling in R. _Journal of Statistical Software_, 70(8), 1 - 33.
#' @examples
#' surv_reg()
#' # Parameters can be represented by a placeholder:
#' surv_reg(dist = varying())
#'
#' @export
surv_reg <-
  function(mode = "regression",
           dist = NULL,
           others = list(),
           ...) {
    check_empty_ellipse(...)
    if (!(mode %in% surv_reg_modes))
      stop(
        "`mode` should be one of: ",
        paste0("'", surv_reg_modes, "'", collapse = ", "),
        call. = FALSE
      )
    args <- list(dist = dist)

    no_value <- !vapply(others, is.null, logical(1))
    others <- others[no_value]

    # write a constructor function
    out <- list(
      args = args,
      others = others,
      mode = mode,
      method = NULL,
      engine = NULL
    )
    class(out) <- make_classes("surv_reg")
    out
  }

#' @export
print.surv_reg <- function(x, ...) {
  cat("Parametric Survival Regression Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if(!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' Update a Parametric Survival Regression Specification
#'
#' If parameters need to be modified, this function can be used
#'  in lieu of recreating the object from scratch.
#'
#' @inheritParams surv_reg
#' @param object A survival regression model specification.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @return An updated model specification.
#' @examples
#' model <- surv_reg(dist = "weibull")
#' model
#' update(model, dist = "lnorm")
#' @method update surv_reg
#' @rdname surv_reg
#' @export
update.surv_reg <-
  function(object,
           dist = NULL,
           others = list(),
           fresh = FALSE,
           ...) {
    check_empty_ellipse(...)

    args <- list(dist = dist)

    if (fresh) {
      object$args <- args
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
    }

    if (length(others) > 0) {
      if (fresh)
        object$others <- others
      else
        object$others[names(others)] <- others
    }

    object
  }


# ------------------------------------------------------------------------------

#' @export
translate.surv_reg <- function(x, engine, ...) {
  x <- translate.default(x, engine, ...)

  if (x$engine == "flexsurv") {
    # `dist` has no default in the function
    if (all(names(x$method$fit$args) != "dist"))
      x$method$fit$args$dist <- "weibull"
  }
  x
}

