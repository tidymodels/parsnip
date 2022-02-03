#' Single layer neural network
#'
#' @description
#' `mlp()` defines a multilayer perceptron model (a.k.a. a single layer,
#' feed-forward neural network).
#'
#' There are different ways to fit this model. The method of estimation is
#' chosen by setting the model _engine_.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("mlp")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams boost_tree
#' @param hidden_units An integer for the number of units in the hidden model.
#' @param penalty A non-negative numeric value for the amount of weight
#'  decay.
#' @param dropout A number between 0 (inclusive) and 1 denoting the proportion
#'  of model parameters randomly set to zero during model training.
#' @param epochs An integer for the number of training iterations.
#' @param activation A single character string denoting the type of relationship
#'  between the original predictors and the hidden unit layer. The activation
#'  function between the hidden and output layers is automatically set to either
#'  "linear" or "softmax" depending on the type of outcome. Possible values are:
#'  "linear", "softmax", "relu", and "elu"
#'
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("mlp")}
#'
#' @examples
#' show_engines("mlp")
#'
#' mlp(mode = "classification", penalty = 0.01)
#' @export

mlp <-
  function(mode = "unknown", engine = "nnet",
           hidden_units = NULL, penalty = NULL, dropout = NULL, epochs = NULL,
           activation = NULL, learn_rate = NULL) {

    args <- list(
      hidden_units = enquo(hidden_units),
      penalty      = enquo(penalty),
      dropout      = enquo(dropout),
      epochs       = enquo(epochs),
      activation   = enquo(activation),
      learn_rate   = enquo(learn_rate)
    )

    new_model_spec(
      "mlp",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
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

#' @method update mlp
#' @rdname parsnip_update
#' @export
update.mlp <-
  function(object,
           parameters = NULL,
           hidden_units = NULL, penalty = NULL, dropout = NULL,
           epochs = NULL, activation = NULL, learn_rate = NULL,
           fresh = FALSE, ...) {

    eng_args <- update_engine_parameters(object$eng_args, ...)

    if (!is.null(parameters)) {
      parameters <- check_final_param(parameters)
    }

    args <- list(
      hidden_units = enquo(hidden_units),
      penalty      = enquo(penalty),
      dropout      = enquo(dropout),
      epochs       = enquo(epochs),
      activation   = enquo(activation),
      learn_rate   = enquo(learn_rate)
    )

    args <- update_main_parameters(args, parameters)

    # TODO make these blocks into a function and document well
    if (fresh) {
      object$args <- args
      object$eng_args <- eng_args
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
      if (length(eng_args) > 0)
        object$eng_args[names(eng_args)] <- eng_args
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
      rlang::abort("The amount of weight decay must be >= 0.")

  if (is.numeric(args$dropout))
    if (args$dropout < 0 | args$dropout >= 1)
      rlang::abort("The dropout proportion must be on [0, 1).")

  if (is.numeric(args$penalty) & is.numeric(args$dropout))
    if (args$dropout > 0 & args$penalty > 0)
      rlang::abort("Both weight decay and dropout should not be specified.")


  if (object$engine == "brulee") {
    act_funs <- c("linear", "relu", "elu", "tanh")
  } else if (object$engine == "keras") {
    act_funs <- c("linear", "softmax", "relu", "elu")
  }

  if (is.character(args$activation)) {
    if (!any(args$activation %in% c(act_funs))) {
      rlang::abort(
        glue::glue("`activation` should be one of: ",
                   glue::glue_collapse(glue::glue("'{act_funs}'"), sep = ", "))
      )
    }
  }

  invisible(object)
}

# keras wrapper for feed-forward nnet

class2ind <- function (x, drop2nd = FALSE) {
  if (!is.factor(x))
    rlang::abort("`x` should be a factor")
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
#' @param penalty A non-negative real number for the amount of weight decay. Either
#'  this parameter _or_ `dropout` can specified.
#' @param dropout The proportion of parameters to set to zero. Either
#'  this parameter _or_ `penalty` can specified.
#' @param epochs An integer for the number of passes through the data.
#' @param activation A character string for the type of activation function between layers.
#' @param seeds A vector of three positive integers to control randomness of the
#'  calculations.
#' @param ... Currently ignored.
#' @return A `keras` model object.
#' @keywords internal
#' @export
keras_mlp <-
  function(x, y,
           hidden_units = 5, penalty = 0, dropout = 0, epochs = 20, activation = "softmax",
           seeds = sample.int(10^5, size = 3),
           ...) {

    if (penalty > 0 & dropout > 0) {
      rlang::abort("Please use either dropoput or weight decay.", call. = FALSE)
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

    if (penalty > 0) {
      model %>%
        keras::layer_dense(
          units = hidden_units,
          activation = activation,
          input_shape = ncol(x),
          kernel_regularizer = keras::regularizer_l2(penalty),
          kernel_initializer = keras::initializer_glorot_uniform(seed = seeds[1])
        )
    } else {
      model %>%
        keras::layer_dense(
          units = hidden_units,
          activation = activation,
          input_shape = ncol(x),
          kernel_initializer = keras::initializer_glorot_uniform(seed = seeds[1])
        )
    }

    if (dropout > 0) {
      model %>%
        keras::layer_dense(
          units = hidden_units,
          activation = activation,
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
    model$y_names <- colnames(y)
    model
  }


nnet_softmax <- function(results, object) {
  if (ncol(results) == 1)
    results <- cbind(1 - results, results)

  results <- apply(results, 1, function(x) exp(x)/sum(exp(x)))
  results <- t(results)
  colnames(results) <- object$lvl
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
  ((p + 1) * hidden_units) + ((hidden_units+1) * classes)
}

## -----------------------------------------------------------------------------

#' @importFrom purrr map_df map
#' @importFrom dplyr arrange select
#' @rdname multi_predict
#' @param epochs An integer vector for the number of training epochs.
#' @export
multi_predict._torch_mlp <-
  function(object, new_data, type = NULL, epochs = NULL, ...) {
    if (any(names(enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

    load_libs(object, quiet = TRUE, attach = TRUE)

    if (is.null(epochs))
      epochs <- length(object$fit$models)

    epochs <- sort(epochs)

    if (is.null(type)) {
      if (object$spec$mode == "classification")
        type <- "class"
      else
        type <- "numeric"
    }

    res <-
      purrr::map(epochs,
                 ~ predict(object, new_data, type, epochs = .x) %>%
                   dplyr::mutate(epochs = .x)) %>%
      purrr::map_dfr(~ .x %>% dplyr::mutate(.row = 1:nrow(new_data))) %>%
      dplyr::arrange(.row, epochs)
    res <- split(dplyr::select(res, -.row), res$.row)
    names(res) <- NULL
    tibble(.pred = res)
  }


reformat_torch_num <- function(results, object) {

  if (isTRUE(ncol(results) > 1)) {
    nms <- colnames(results)
    results <- as_tibble(results, .name_repair = "minimal")
    if (length(nms) == 0 && length(object$preproc$y_var) == ncol(results)) {
      names(results) <- object$preproc$y_var
    }
  }  else {
    results <- unname(results[[1]])
  }
  results
}

#' Wrapper for keras class predictions
#' @param object A keras model fit
#' @param x A data set.
#' @export
#' @keywords internal
keras_predict_classes <- function(object, x)  {
  if (utils::packageVersion("keras") >= package_version("2.6")) {
    object %>% predict(x) %>% keras::k_argmax() %>% as.integer()
  } else {
    keras::predict_classes(object, x)
  }
}

#' Wrapper for keras class probability predictions
#' @param object A keras model fit
#' @param x A data set.
#' @export
#' @keywords internal
keras_predict_proba <- function(object, x)  {
  if (utils::packageVersion("keras") >= package_version("2.6")) {
    object %>% predict(x)
  } else {
    keras::predict_proba(object, x)
  }

}
