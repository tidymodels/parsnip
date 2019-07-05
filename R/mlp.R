#' General Interface for Single Layer Neural Network
#'
#' `mlp()`, for multilayer perceptron, is a way to generate a _specification_ of
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
#'  set using `set_engine()`. If left to their defaults
#'  here (see above), the values are taken from the underlying model
#'  functions. One exception is `hidden_units` when `nnet::nnet` is used; that
#'  function's `size` argument has no default so a value of 5 units will be
#'  used. Also, unless otherwise specified, the `linout` argument to
#'  `nnet::nnet()` will be set to `TRUE` when a regression model is created.
#'  If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#' @inheritParams boost_tree
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
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
#' @details
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"nnet"` (the default)
#' \item \pkg{keras}: `"keras"`
#' }
#'
#'  An error is thrown if both `penalty` and `dropout` are specified for
#'  `keras` models.
#'
#' @section Engine Details:
#'
#' Engines may have pre-set default arguments when executing the
#'  model fit call. For this type of
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
           activation = NULL) {

    args <- list(
      hidden_units = enquo(hidden_units),
      penalty      = enquo(penalty),
      dropout      = enquo(dropout),
      epochs       = enquo(epochs),
      activation   = enquo(activation)
    )

    new_model_spec(
      "mlp",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
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
#' @inheritParams update.boost_tree
#' @param object A random forest model specification.
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
           fresh = FALSE, ...) {
    update_dot_check(...)
    args <- list(
      hidden_units = enquo(hidden_units),
      penalty      = enquo(penalty),
      dropout      = enquo(dropout),
      epochs       = enquo(epochs),
      activation   = enquo(activation)
    )

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

    new_model_spec(
      "mlp",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

#' @export
translate.mlp <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'keras'` for translation.")
    engine <- "keras"
  }

  if (engine == "nnet") {
    if(isTRUE(is.null(quo_get_expr(x$args$hidden_units)))) {
      x$args$hidden_units <- 5
    }
  }

  x <- translate.default(x, engine, ...)

  if (engine == "nnet") {
    if (x$mode == "classification") {
      if (length(x$eng_args) == 0  || !any(names(x$eng_args) == "linout"))
        x$method$fit$args$linout <- FALSE
    } else {
      if (length(x$eng_args) == 0  || !any(names(x$eng_args) == "linout"))
        x$method$fit$args$linout <- TRUE
    }
  }
  x
}

# ------------------------------------------------------------------------------

check_args.mlp <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (is.numeric(args$penalty))
    if (args$penalty < 0)
      stop("The amount of weight decay must be >= 0.", call. = FALSE)

  if (is.numeric(args$dropout))
    if (args$dropout < 0 | args$dropout >= 1)
      stop("The dropout proportion must be on [0, 1).", call. = FALSE)

  if (is.numeric(args$penalty) & is.numeric(args$dropout))
    if (args$dropout > 0 & args$penalty > 0)
      stop("Both weight decay and dropout should not be specified.", call. = FALSE)

  act_funs <- c("linear", "softmax", "relu", "elu")

  if (is.character(args$activation))
    if (!any(args$activation %in% c(act_funs)))
      stop("`activation should be one of: ",
           paste0("'", act_funs, "'", collapse = ", "),
           call. = FALSE)

  invisible(object)
}

# keras wrapper for feed-forward nnet

class2ind <- function (x, drop2nd = FALSE) {
  if (!is.factor(x))
    stop("`x` should be a factor")
  y <- model.matrix( ~ x - 1)
  colnames(y) <- gsub("^x", "", colnames(y))
  attributes(y)$assign <- NULL
  attributes(y)$contrasts <- NULL
  if (length(levels(x)) == 2 & drop2nd) {
    y <- y[, 1]
  }
  y
}


# ------------------------------------------------------------------------------

#' Simple interface to MLP models via keras
#'
#' Instead of building a `keras` model sequentially, `keras_mlp` can be used to
#'  create a feedforward network with a single hidden layer. Regularization is
#'  via either weight decay or dropout.
#'
#' @param x A data frame or matrix of predictors
#' @param y A vector (factor or numeric) or matrix (numeric) of outcome data.
#' @param hidden_units An integer for the number of hidden units.
#' @param decay A non-negative real number for the amount of weight decay. Either
#'  this parameter _or_ `dropout` can specified.
#' @param dropout The proportion of parameters to set to zero. Either
#'  this parameter _or_ `decay` can specified.
#' @param epochs An integer for the number of passes through the data.
#' @param act A character string for the type of activation function between layers.
#' @param seeds A vector of three positive integers to control randomness of the
#'  calculations.
#' @param ... Currently ignored.
#' @return A `keras` model object.
#' @keywords internal
#' @export
keras_mlp <-
  function(x, y,
           hidden_units = 5, decay = 0, dropout = 0, epochs = 20, act = "softmax",
           seeds = sample.int(10^5, size = 3),
           ...) {

    if (decay > 0 & dropout > 0) {
      stop("Please use either dropoput or weight decay.", call. = FALSE)
    }
    if (!is.matrix(x)) {
      x <- as.matrix(x)
    }

    if (is.character(y)) {
      y <- as.factor(y)
    }
    factor_y <- is.factor(y)

    if (factor_y) {
      y <- class2ind(y)
    } else {
      if (isTRUE(ncol(y) > 1)) {
        y <- as.matrix(y)
      } else {
        y <- matrix(y, ncol = 1)
      }
    }

    model <- keras::keras_model_sequential()

    if (decay > 0) {
      model %>%
        keras::layer_dense(
          units = hidden_units,
          activation = act,
          input_shape = ncol(x),
          kernel_regularizer = keras::regularizer_l2(decay),
          kernel_initializer = keras::initializer_glorot_uniform(seed = seeds[1])
        )
    } else {
      model %>%
        keras::layer_dense(
          units = hidden_units,
          activation = act,
          input_shape = ncol(x),
          kernel_initializer = keras::initializer_glorot_uniform(seed = seeds[1])
        )
    }

    if (dropout > 0) {
      model %>%
        keras::layer_dense(
          units = hidden_units,
          activation = act,
          input_shape = ncol(x),
          kernel_initializer = keras::initializer_glorot_uniform(seed = seeds[1])
        ) %>%
        keras::layer_dropout(rate = dropout, seed = seeds[2])
    }

    if (factor_y) {
      model <- model %>%
        keras::layer_dense(
          units = ncol(y),
          activation = 'softmax',
          kernel_initializer = keras::initializer_glorot_uniform(seed = seeds[3])
        )
    } else {
      model <- model %>%
        keras::layer_dense(
          units = ncol(y),
          activation = 'linear',
          kernel_initializer = keras::initializer_glorot_uniform(seed = seeds[3])
        )
    }

    arg_values <- parse_keras_args(...)
    compile_call <- expr(keras::compile(object = model))
    if (!any(names(arg_values$compile) == "loss")) {
      if (factor_y) {
        compile_call$loss <- "binary_crossentropy"
      } else {
        compile_call$loss <- "mse"
      }
    }

    if (!any(names(arg_values$compile) == "optimizer")) {
      compile_call$optimizer <- "adam"
    }

    compile_call <- rlang::call_modify(compile_call, !!!arg_values$compile)

    model <- eval_tidy(compile_call)

    fit_call <- expr(keras::fit(object = model))
    fit_call$x <- quote(x)
    fit_call$y <- quote(y)
    fit_call$epochs <- epochs
    fit_call <- rlang::call_modify(fit_call, !!!arg_values$fit)

    history <- eval_tidy(fit_call)
    model
  }


nnet_softmax <- function(results, object) {
  if (ncol(results) == 1)
    results <- cbind(1 - results, results)

  results <- apply(results, 1, function(x) exp(x)/sum(exp(x)))
  results <- t(results)
  names(results) <- paste0(".pred_", object$lvl)
  results <- as_tibble(results)
  results
}

parse_keras_args <- function(...) {
  exclusions <- c("object", "x", "y", "validation_data", "epochs")
  fit_args <- c(
    'batch_size',
    'verbose',
    'callbacks',
    'view_metrics',
    'validation_split',
    'validation_data',
    'shuffle',
    'class_weight',
    'sample_weight',
    'initial_epoch',
    'steps_per_epoch',
    'validation_steps'
  )
  compile_args <- c(
    'optimizer',
    'loss',
    'metrics',
    'loss_weights',
    'sample_weight_mode',
    'weighted_metrics',
    'target_tensors'
  )
  dots <- list(...)
  dots <- dots[!(names(dots) %in% exclusions)]

  list(
    fit = dots[names(dots) %in% fit_args],
    compile = dots[names(dots) %in% compile_args]
  )
}

mlp_num_weights <- function(p, hidden_units, classes) {
  ((p+1) * hidden_units) + ((hidden_units+1) * classes)
}


