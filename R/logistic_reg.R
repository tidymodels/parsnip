#' General Interface for Logistic Regression Models
#'
#' `logistic_reg` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R, Stan, or via Spark. The main arguments for the
#'  model are:
#' \itemize{
#'   \item \code{penalty}: The total amount of regularization
#'  in the model. Note that this must be zero for some engines.
#'   \item \code{mixture}: The proportion of L1 regularization in
#'  the model. Note that this will be ignored for some engines.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using the `others` argument. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions.
#'
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `logistic_reg`,the
#'  mode will always be "classification".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"glm"` or `"glmnet"`
#' \item \pkg{Stan}:  `"stan"`
#' \item \pkg{Spark}: `"spark"`
#' }
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "classification".
#' @param others A named list of arguments to be used by the
#'  underlying models (e.g., `stats::glm`,
#'  `rstanarm::stan_glm`, etc.). These are not evaluated
#'  until the model is fit and will be substituted into the model
#'  fit expression.
#' @param penalty An non-negative number representing the
#'  total amount of regularization (`glmnet` and `spark` only).
#' @param mixture A number between zero and one (inclusive) that
#'  represents the proportion of regularization that is used for the
#'  L2 penalty (i.e. weight decay, or ridge regression) versus L1
#'  (the lasso) (`glmnet` and `spark` only).
#' @param ... Used for S3 method consistency. Any arguments passed to
#'  the ellipses will result in an error. Use `others` instead.
#' @seealso [varying()], [fit()]
#' @examples
#' logistic_reg()
#' # Parameters can be represented by a placeholder:
#' logistic_reg(penalty = varying())
#' @export
#' @importFrom purrr map_lgl
logistic_reg <-
  function(mode = "classification",
           penalty = NULL,
           mixture = NULL,
           others = list(),
           ...) {
    check_empty_ellipse(...)
    if (!(mode %in% logistic_reg_modes))
      stop(
        "`mode` should be one of: ",
        paste0("'", logistic_reg_modes, "'", collapse = ", "),
        call. = FALSE
      )

    if (is.numeric(penalty) && penalty < 0)
      stop("The amount of regularization should be >= 0", call. = FALSE)
    if (is.numeric(mixture) && (mixture < 0 | mixture > 1))
      stop("The mixture proportion should be within [0,1]", call. = FALSE)

    args <- list(penalty = penalty, mixture = mixture)

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
    class(out) <- make_classes("logistic_reg")
    out
  }

#' @export
print.logistic_reg <- function(x, ...) {
  cat("Logistic Regression Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if(!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

###################################################################

#' Update a Logistic Regression Specification
#'
#' If parameters need to be modified, this function can be used
#'  in lieu of recreating the object from scratch.
#'
#' @inheritParams logistic_reg
#' @param object A logistic regression model specification.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @return An updated model specification.
#' @examples
#' model <- logistic_reg(penalty = 10, mixture = 0.1)
#' model
#' update(model, penalty = 1)
#' update(model, penalty = 1, fresh = TRUE)
#' @method update logistic_reg
#' @rdname logistic_reg
#' @export
update.logistic_reg <-
  function(object,
           penalty = NULL, mixture = NULL,
           others = list(),
           fresh = FALSE,
           ...) {
    check_empty_ellipse(...)

    if (is.numeric(penalty) && penalty < 0)
      stop("The amount of regularization should be >= 0", call. = FALSE)
    if (is.numeric(mixture) && (mixture < 0 | mixture > 1))
      stop("The mixture proportion should be within [0,1]", call. = FALSE)

    args <- list(penalty = penalty, mixture = mixture)

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

