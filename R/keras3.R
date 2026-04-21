#' Activation functions for neural networks in keras3
#'
#' @keywords internal
#' @return A character vector of values.
#' @export
keras3_activations <- function() {
  allowed_keras_activation
}

#' Simple interface to MLP models via keras3
#'
#' Instead of building a `keras3` model sequentially, `keras3_mlp` can be used
#' to create a feedforward network with a single hidden layer. Regularization
#' is via either weight decay or dropout.
#'
#' @param x A data frame or matrix of predictors.
#' @param y A vector (factor or numeric) or matrix (numeric) of outcome data.
#' @param hidden_units An integer for the number of hidden units.
#' @param penalty A non-negative real number for the amount of weight decay.
#'   Either this parameter _or_ `dropout` can be specified.
#' @param dropout The proportion of parameters to set to zero. Either this
#'   parameter _or_ `penalty` can be specified.
#' @param epochs An integer for the number of passes through the data.
#' @param activation A character string for the type of activation function
#'   between layers.
#' @param seed A single positive integer to control randomness.
#' @param ... Additional named arguments to pass to `keras3::compile()` or
#'   `keras3::fit()`.
#' @return A `keras3` model object.
#' @keywords internal
#' @export
keras3_mlp <-
  function(
    x,
    y,
    hidden_units = 5,
    penalty = 0,
    dropout = 0,
    epochs = 20,
    activation = "softmax",
    seed = sample.int(10^5, size = 1),
    ...
  ) {
    allowed <- keras3_activations()
    if (!all(activation %in% allowed)) {
      cli::cli_abort(
        "{.arg activation} should be one of: {allowed}, not {.val {activation}}."
      )
    }
    activation <- get_activation_fn(activation)

    if (penalty > 0 & dropout > 0) {
      cli::cli_abort("Please use either dropout or weight decay.", call = NULL)
    }

    keras3::set_random_seed(seed)

    if (!is.matrix(x)) {
      x <- as.matrix(x)
    }
    if (is.character(y)) {
      y <- as.factor(y)
    }
    factor_y <- is.factor(y)
    binary_y <- factor_y && nlevels(y) == 2L

    if (factor_y) {
      if (binary_y) {
        y_mat <- class2ind(y, drop2nd = TRUE)
        y_mat <- matrix(y_mat, ncol = 1)
      } else {
        y_mat <- class2ind(y)
      }
    } else {
      if (isTRUE(ncol(y) > 1)) {
        y_mat <- as.matrix(y)
      } else {
        y_mat <- matrix(y, ncol = 1)
      }
    }

    regularizer <- if (penalty > 0) keras3::regularizer_l2(penalty) else NULL

    model <- keras3::keras_model_sequential(input_shape = ncol(x))

    model |>
      keras3::layer_dense(
        units = hidden_units,
        activation = activation,
        kernel_regularizer = regularizer
      )

    if (dropout > 0) {
      model |> keras3::layer_dropout(rate = dropout)
    }

    if (binary_y) {
      model |> keras3::layer_dense(units = 1L, activation = "sigmoid")
    } else if (factor_y) {
      model |>
        keras3::layer_dense(units = ncol(y_mat), activation = "softmax")
    } else {
      model |>
        keras3::layer_dense(units = ncol(y_mat), activation = "linear")
    }

    arg_values <- parse_keras3_args(...)
    compile_call <- rlang::expr(keras3::compile(object = model))

    if (!any(names(arg_values$compile) == "loss")) {
      if (binary_y) {
        compile_call$loss <- "binary_crossentropy"
      } else if (factor_y) {
        compile_call$loss <- "categorical_crossentropy"
      } else {
        compile_call$loss <- "mse"
      }
    }
    if (!any(names(arg_values$compile) == "optimizer")) {
      compile_call$optimizer <- "adam"
    }
    compile_call <- rlang::call_modify(compile_call, !!!arg_values$compile)
    model <- rlang::eval_tidy(compile_call)

    fit_call <- rlang::expr(keras3::fit(object = model))
    fit_call$x <- quote(x)
    fit_call$y <- quote(y_mat)
    fit_call$epochs <- epochs
    fit_call <- rlang::call_modify(fit_call, !!!arg_values$fit)
    rlang::eval_tidy(fit_call)

    model$y_names <- colnames(y_mat)
    model
  }

parse_keras3_args <- function(...) {
  exclusions <- c("object", "x", "y", "validation_data", "epochs")
  fit_args <- c(
    "batch_size",
    "verbose",
    "callbacks",
    "validation_split",
    "validation_data",
    "shuffle",
    "class_weight",
    "sample_weight",
    "initial_epoch",
    "steps_per_epoch",
    "validation_steps"
  )
  compile_args <- c(
    "optimizer",
    "loss",
    "metrics",
    "loss_weights",
    "weighted_metrics",
    "run_eagerly",
    "steps_per_execution",
    "jit_compile"
  )
  dots <- list(...)
  dots <- dots[!(names(dots) %in% exclusions)]
  list(
    fit = dots[names(dots) %in% fit_args],
    compile = dots[names(dots) %in% compile_args]
  )
}

#' Wrapper for keras3 class predictions
#' @param object A keras3 model fit
#' @param x A data set.
#' @export
#' @keywords internal
keras3_predict_classes <- function(object, x) {
  pred <- predict(object$fit, x)
  if (ncol(pred) == 1L) {
    object$lvl[as.integer(pred[, 1] > 0.5) + 1L]
  } else {
    object$lvl[as.integer(keras3::op_argmax(pred, axis = 2L)) + 1L]
  }
}
