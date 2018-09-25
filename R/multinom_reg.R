#' General Interface for Multinomial Regression Models
#'
#' `multinom_reg` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R or Spark. The main arguments for the
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
#'  functions. If parameters need to be modified, `update` can be used
#'  in lieu of recreating the object from scratch.
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "classification".
#' @param others A named list of arguments to be used by the
#'  underlying models (e.g., `glmnet::glmnet` etc.). These are not evaluated
#'  until the model is fit and will be substituted into the model
#'  fit expression.
#' @param penalty An non-negative number representing the
#'  total amount of regularization.
#' @param mixture A number between zero and one (inclusive) that
#'  represents the proportion of regularization that is used for the
#'  L2 penalty (i.e. weight decay, or ridge regression) versus L1
#'  (the lasso) (`glmnet` only).
#' @param ... Used for S3 method consistency. Any arguments passed to
#'  the ellipses will result in an error. Use `others` instead.
#' @details
#' For `multinom_reg`, the mode will always be "classification".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:   `"glmnet"`
#' \item \pkg{Stan}:  `"stan"`
#' }
#'
#' Engines may have pre-set default arguments when executing the
#'  model fit call. These can be changed by using the `others`
#'  argument to pass in the preferred values. For this type of
#'  model, the template of the fit calls are:
#'
#' \pkg{glmnet}
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::multinom_reg(), "glmnet")}
#'
#' \pkg{spark}
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::multinom_reg(), "spark")}
#'
#' When using `glmnet` models, there is the option to pass
#'  multiple values (or no values) to the `penalty` argument.
#'  This can have an effect on the model object results. When using
#'  the `predict` method in these cases, the return object type
#'  depends on the value of `penalty`. If a single value is
#'  given, the results will be a simple numeric vector. When
#'  multiple values or no values for `penalty` are used in
#'  `multinom_reg`, the `predict` method will return a data frame with
#'  columns `values` and `lambda`.
#'
#' @note For models created using the spark engine, there are
#'  several differences to consider. First, only the formula
#'  interface to via `fit` is available; using `fit_xy` will
#'  generate an error. Second, the predictions will always be in a
#'  spark table format. The names will be the same as documented but
#'  without the dots. Third, there is no equivalent to factor
#'  columns in spark tables so class predictions are returned as
#'  character columns. Fourth, to retain the model object for a new
#'  R session (via `save`), the `model$fit` element of the `parsnip`
#'  object should be serialized via `ml_save(object$fit)` and
#'  separately saved to disk. In a new session, the object can be
#'  reloaded and reattached to the `parsnip` object.
#'
#' @seealso [varying()], [fit()]
#' @examples
#' multinom_reg()
#' # Parameters can be represented by a placeholder:
#' multinom_reg(penalty = varying())
#' @export
#' @importFrom purrr map_lgl
multinom_reg <-
  function(mode = "classification",
           penalty = NULL,
           mixture = NULL,
           others = list(),
           ...) {
    check_empty_ellipse(...)
    if (!(mode %in% multinom_reg_modes))
      stop(
        "`mode` should be one of: ",
        paste0("'", multinom_reg_modes, "'", collapse = ", "),
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
    class(out) <- make_classes("multinom_reg")
    out
  }

#' @export
print.multinom_reg <- function(x, ...) {
  cat("Multinomial Regression Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if(!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @inheritParams multinom_reg
#' @param object A multinomial regression model specification.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @return An updated model specification.
#' @examples
#' model <- multinom_reg(penalty = 10, mixture = 0.1)
#' model
#' update(model, penalty = 1)
#' update(model, penalty = 1, fresh = TRUE)
#' @method update multinom_reg
#' @rdname multinom_reg
#' @export
update.multinom_reg <-
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


# ------------------------------------------------------------------------------

organize_multnet_class <- function(x, object) {
  x[,1]
}

organize_multnet_prob <- function(x, object) {
  x <- x[,,1]
  x <- as_tibble(x)
  names(x) <- paste0(".pred_", names(x))
  x
}

# ------------------------------------------------------------------------------

#' @export
predict._multnet <-
  function(object, new_data, type = NULL, opts = list(), penalty = NULL, ...) {
    dots <- list(...)
    if (is.null(penalty))
      penalty <- object$fit$lambda

  if (length(penalty) != 1)
    stop("`penalty` should be a single numeric value. ",
         "`multi_predict` can be used to get multiple predictions ",
         "per row of data.", call. = FALSE)
    res <- predict.model_fit(
      object = object,
      new_data = new_data,
      type = type,
      opts = opts,
      penalty = penalty
    )
  res
}


#' @importFrom dplyr full_join as_tibble arrange
#' @importFrom tidyr gather
#' @export
multi_predict._multnet <-
  function(object, new_data, type = NULL, penalty = NULL, ...) {
    dots <- list(...)
    if (is.null(penalty))
      penalty <- object$lambda

    if (is.null(type))
      type <- "class"
    if (!(type %in% c("class", "prob", "link"))) {
      stop ("`type` should be either 'class', 'link', or 'prob'.", call. = FALSE)
    }
    if (type == "prob")
      dots$type <- "response"
    else
      dots$type <- type

    dots$s <- penalty
    pred <- predict.model_fit(object, new_data = new_data, type = "raw", opts = dots)

    format_probs <- function(x) {
      x <- as_tibble(x)
      names(x) <- paste0(".pred_", names(x))
      nms <- names(x)
      x$.row <- 1:nrow(x)
      x[, c(".row", nms)]
    }

    if (type == "prob") {
      pred <- apply(pred, 3, format_probs)
      names(pred) <- NULL
      pred <- map_dfr(pred, function(x) x)
      pred$penalty <- rep(penalty, each = nrow(new_data))
    } else {
      pred <-
        tibble(
          .row = rep(1:nrow(new_data), length(penalty)),
          .pred = as.vector(pred),
          penalty = rep(penalty, each = nrow(new_data))
        )
    }

    pred <- arrange(pred, .row, penalty)
    .row <- pred$.row
    pred$.row <- NULL
    pred <- split(pred, .row)
    names(pred) <- NULL
    tibble(.pred = pred)
  }

check_glmnet_lambda <- function(dat, object) {
  if (length(object$fit$lambda) > 1)
    stop(
      "`predict` doesn't work with multiple penalties (i.e. lambdas). ",
      "Please specify a single value using `penalty = some_value` or use ",
      "`multi_predict` to get multiple predictions per row of data.",
      call. = FALSE
    )
  dat
}

# ------------------------------------------------------------------------------

#' @importFrom utils globalVariables
utils::globalVariables(c("group", ".pred"))
