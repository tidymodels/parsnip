
mlp_arg_key <- data.frame(
  nnet = c("size", "decay", NA_character_, "maxit", NA_character_),
  keras = c("hidden_units", "penalty", "dropout", "epochs", "activation"),
  stringsAsFactors = FALSE,
  row.names =  c("hidden_units", "penalty", "dropout", "epochs", "activation")
)

mlp_modes <- c("classification", "regression", "unknown")

mlp_engines <- data.frame(
  nnet  = c(TRUE, TRUE, FALSE),
  keras = c(TRUE, TRUE, FALSE),
  row.names =  c("classification", "regression", "unknown")
)

# ------------------------------------------------------------------------------

mlp_keras_data <-
  list(
    libs = c("keras", "magrittr"),
    fit = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(pkg = "parsnip", fun = "keras_mlp"),
      defaults = list()
    ),
    numeric = list(
      pre = NULL,
      post = maybe_multivariate,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          x = quote(as.matrix(new_data))
        )
    ),
    class = list(
      pre = NULL,
      post = function(x, object) {
        object$lvl[x + 1]
      },
      func = c(pkg = "keras", fun = "predict_classes"),
      args =
        list(
          object = quote(object$fit),
          x = quote(as.matrix(new_data))
        )
    ),
    classprob = list(
      pre = NULL,
      post = function(x, object) {
        x <- as_tibble(x)
        colnames(x) <- object$lvl
        x
      },
      func = c(pkg = "keras", fun = "predict_proba"),
      args =
        list(
          object = quote(object$fit),
          x = quote(as.matrix(new_data))
        )
    ),
    raw = list(
      pre = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          x = quote(as.matrix(new_data))
        )
    )
  )


nnet_softmax <- function(results, object) {
  if (ncol(results) == 1)
    results <- cbind(1 - results, results)
    
  results <- apply(results, 1, function(x) exp(x)/sum(exp(x)))
  results <- as_tibble(t(results))
  names(results) <- paste0(".pred_", object$lvl)
  results
}

mlp_nnet_data <-
  list(
    libs = "nnet",
    fit = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "nnet", fun = "nnet"),
      defaults = list(trace = FALSE)
    ),
    numeric = list(
      pre = NULL,
      post = maybe_multivariate,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "raw"
        )
    ),
    class = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "class"
        )
    ),
    classprob = list(
      pre = NULL,
      post = nnet_softmax,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "raw"
        )
    ),
    raw = list(
      pre = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )


# ------------------------------------------------------------------------------

# keras wrapper for feed-forward nnet

class2ind <- function (x, drop2nd = FALSE) {
  if (!is.factor(x))
    stop("'x' should be a factor")
  y <- model.matrix( ~ x - 1)
  colnames(y) <- gsub("^x", "", colnames(y))
  attributes(y)$assign <- NULL
  attributes(y)$contrasts <- NULL
  if (length(levels(x)) == 2 & drop2nd) {
    y <- y[, 1]
  }
  y
}


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
#' @export
keras_mlp <-
  function(x, y,
           hidden_units = 5, decay = 0, dropout = 0, epochs = 20, act = "softmax",
           seeds = sample.int(10^5, size = 3),
           ...) {

    if(decay > 0 & dropout > 0)
      stop("Please use either dropoput or weight decay.", call. = FALSE)

    if (!is.matrix(x))
      x <- as.matrix(x)

    if(is.character(y))
      y <- as.factor(y)
    factor_y <- is.factor(y)

    if (factor_y)
      y <- class2ind(y)
    else {
      if (isTRUE(ncol(y) > 1))
        y <- as.matrix(y)
      else
        y <- matrix(y, ncol = 1)
    }

    model <- keras::keras_model_sequential()
    if(decay > 0) {
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
    if(dropout > 0)
      model %>%
      keras::layer_dense(
        units = hidden_units,
        activation = act,
        input_shape = ncol(x),
        kernel_initializer = keras::initializer_glorot_uniform(seed = seeds[1])
      ) %>%
      keras::layer_dropout(rate = dropout, seed = seeds[2])

    if (factor_y)
      model <- model %>%
      keras::layer_dense(
        units = ncol(y),
        activation = 'softmax',
        kernel_initializer = keras::initializer_glorot_uniform(seed = seeds[3])
      )
    else
      model <- model %>%
      keras::layer_dense(
        units = ncol(y),
        activation = 'linear',
        kernel_initializer = keras::initializer_glorot_uniform(seed = seeds[3])
      )

    arg_values <- parse_keras_args(...)
    compile_call <- expr(
      keras::compile(object = model)
    )
    if(!any(names(arg_values$compile) == "loss"))
      compile_call$loss <-
      if(factor_y) "binary_crossentropy" else "mse"
    if(!any(names(arg_values$compile) == "optimizer"))
      compile_call$optimizer <- "adam"
    for(arg in names(arg_values$compile))
      compile_call[[arg]] <- arg_values$compile[[arg]]

    model <- eval_tidy(compile_call)

    fit_call <- expr(
      keras::fit(object = model)
    )
    fit_call$x <- quote(x)
    fit_call$y <- quote(y)
    fit_call$epochs <- epochs
    for(arg in names(arg_values$fit))
      fit_call[[arg]] <- arg_values$fit[[arg]]

    history <- eval_tidy(fit_call)
    model
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

mlp_num_weights <- function(p, hidden_units, classes)
 ((p+1) * hidden_units) + ((hidden_units+1) * classes)



