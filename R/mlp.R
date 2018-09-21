#' General Interface for Single Layer Neural Network
#'
#' `mlp`, for multilayer perceptron, is a way to generate a _specification_ of
#'  a model before fitting and allows the model to be created using
#'  different packages in R or via keras The main arguments for the
#'  model are:
#' \itemize{
#'   \item \code{hidden_units}: The number of units in the hidden layer
#'    (default: 5).
#'   \item \code{penalty}: The amount of L2 regularization (aka weight
#'     decay, default is zero).
#'   \item \code{dropout}: The proportion of parameters randomly dropped out of
#'     the model (`keras` only, default is zero).
#'   \item \code{epochs}: The number of training iterations (default: 20).
#'   \item \code{activation}: The type of function that connects the hidden
#'     layer and the input variables  (`keras` only, default is softmax).
#' }
#'
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using the `others` argument. If left to their defaults
#'  here (see above), the values are taken from the underlying model
#'  functions. One exception is `hidden_units` when `nnet::nnet` is used; that
#'  function's `size` argument has no default so a value of 5 units will be
#'  used. Also, unless otherwise specified, the `linout` argument to
#'  `nnet::nnet` will be set to `TRUE` when a regression model is created.
#'  If parameters need to be modified, `update` can be used
#'  in lieu of recreating the object from scratch.

#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#' @param others A named list of arguments to be used by the
#'  underlying models (e.g., `nnet::nnet`,
#'  `keras::fit`, `keras::compile`, etc.). .
#' @param hidden_units An integer for the number of units in the hidden model.
#' @param penalty A non-negative numeric value for the amount of weight
#'  decay.
#' @param dropout A number between 0 (inclusive) and 1 denoting the proportion
#'  of model parameters randomly set to zero during model training.
#' @param epochs An integer for the number of training iterations.
#' @param activation A single character strong denoting the type of relationship
#'  between the original predictors and the hidden unit layer. The activation
#'  function between the hidden and output layers is automatically set to either
#'  "linear" or "softmax" depending on the type of outcome. Possible values are:
#'  "linear", "softmax", "relu", and "elu"
#' @param ... Used for method consistency. Any arguments passed to
#'  the ellipses will result in an error. Use `others` instead.
#' @details
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"nnet"`
#' \item \pkg{keras}: `"keras"`
#' }
#'
#' Main parameter arguments (and those in `others`) can avoid
#'  evaluation until the underlying function is executed by wrapping the
#'  argument in [rlang::expr()] (e.g. `hidden_units = expr(num_preds * 2)`).
#'
#'  An error is thrown if both `penalty` and `dropout` are specified for
#'  `keras` models.
#'
#' Engines may have pre-set default arguments when executing the
#'  model fit call. These can be changed by using the `others`
#'  argument to pass in the preferred values. For this type of
#'  model, the template of the fit calls are:
#'
#' \pkg{keras} classification
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::mlp(mode = "classification"), "keras")}
#'
#' \pkg{keras} regression
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::mlp(mode = "regression"), "keras")}
#'
#' \pkg{nnet} classification
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::mlp(mode = "classification"), "nnet")}
#'
#' \pkg{nnet} regression
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::mlp(mode = "regression"), "nnet")}
#'
#' @importFrom purrr map_lgl
#' @seealso [varying()], [fit()]
#' @examples
#' mlp(mode = "classification", penalty = 0.01)
#' # Parameters can be represented by a placeholder:
#' mlp(mode = "regression", hidden_units = varying())
#' @export

mlp <-
  function(mode = "unknown",
           hidden_units = NULL, penalty = NULL, dropout = NULL, epochs = NULL,
           activation = NULL,
           others = list(),
           ...) {
    check_empty_ellipse(...)

    act_funs <- c("linear", "softmax", "relu", "elu")
    if (is.numeric(hidden_units))
      if (hidden_units < 2)
        stop("There must be at least two hidden units", call. = FALSE)
    if (is.numeric(penalty))
      if (penalty < 0)
        stop("The amount of weight decay must be >= 0.", call. = FALSE)
    if (is.numeric(dropout))
      if (dropout < 0 | dropout >= 1)
        stop("The dropout proportion must be on [0, 1).", call. = FALSE)
    if (is.numeric(penalty) & is.numeric(dropout))
      if (dropout > 0 & penalty > 0)
        stop("Both weight decay and dropout should not be specified.", call. = FALSE)
    if (is.character(activation))
      if (!any(activation %in% c(act_funs)))
        stop("`activation should be one of: ",
             paste0("'", act_funs, "'", collapse = ", "),
             call. = FALSE)

    if (!(mode %in% mlp_modes))
      stop("`mode` should be one of: ",
           paste0("'", mlp_modes, "'", collapse = ", "),
           call. = FALSE)

    args <- list(hidden_units = hidden_units, penalty = penalty, dropout = dropout,
                 epochs = epochs, activation = activation)

    no_value <- !vapply(others, is.null, logical(1))
    others <- others[no_value]

    # write a constructor function
    out <- list(args = args, others = others,
                mode = mode, method = NULL, engine = NULL)
    # TODO: make_classes has wrong order; go from specific to general
    class(out) <- make_classes("mlp")
    out
  }

#' @export
print.mlp <- function(x, ...) {
  cat("Single Layer Neural Network Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if(!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

#' Update a Single Layer Neural Network Specification
#'
#' If parameters need to be modified, this function can be used
#'  in lieu of recreating the object from scratch.
#'
#' @export
#' @inheritParams mlp
#' @param object A random forest model specification.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @return An updated model specification.
#' @examples
#' model <- mlp(hidden_units = 10, dropout = 0.30)
#' model
#' update(model, hidden_units = 2)
#' update(model, hidden_units = 2, fresh = TRUE)
#' @method update mlp
#' @rdname mlp
#' @export
update.mlp <-
  function(object,
           hidden_units = NULL, penalty = NULL, dropout = NULL,
           epochs = NULL, activation = NULL,
           others = list(),
           fresh = FALSE,
           ...) {
    check_empty_ellipse(...)

    args <- list(hidden_units = hidden_units, penalty = penalty, dropout = dropout,
                 epochs = epochs, activation = activation)

    # TODO make these blocks into a function and document well
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
translate.mlp <- function(x, engine, ...) {

  if (engine == "nnet") {
    if(is.null(x$args$hidden_units))
      x$args$hidden_units <- 5
  }

  x <- translate.default(x, engine, ...)

  if (engine == "nnet") {
    if (x$mode == "classification") {
      if (length(x$others) == 0  || !any(names(x$others) == "linout"))
        x$method$fit$args$linout <- FALSE
    } else {
      if (length(x$others) == 0  || !any(names(x$others) == "linout"))
        x$method$fit$args$linout <- TRUE
    }
  }
  x
}
