#' General Interface for Multinomial Regression Models
#'
#' `multinom_reg()` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R, keras, or Spark. The main arguments for the
#'  model are:
#' \itemize{
#'   \item \code{penalty}: The total amount of regularization
#'  in the model. Note that this must be zero for some engines.
#'   \item \code{mixture}: The mixture amounts of different types of
#'   regularization (see below). Note that this will be ignored for some engines.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and arguments can be
#'  set using `set_engine()`. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions. If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#' @inheritParams boost_tree
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "classification".
#' @param penalty A non-negative number representing the total
#'  amount of regularization (`glmnet`, `keras`, and `spark` only).
#'  For `keras` models, this corresponds to purely L2 regularization
#'  (aka weight decay) while the other models can be a combination
#'  of L1 and L2 (depending on the value of `mixture`).
#' @param mixture A number between zero and one (inclusive) that is the
#'  proportion of L1 regularization (i.e. lasso) in the model. When
#'  `mixture = 1`, it is a pure lasso model while `mixture = 0` indicates that
#'  ridge regression is being used. (`glmnet` and `spark` only).
#' @details
#' For `multinom_reg()`, the mode will always be "classification".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:   `"glmnet"`  (the default), `"nnet"`
#' \item \pkg{Stan}:  `"stan"`
#' \item \pkg{keras}: `"keras"`
#' }
#'
#' @includeRmd man/rmd/multinom-reg.Rmd details
#'
#' @note For models created using the spark engine, there are
#'  several differences to consider. First, only the formula
#'  interface to via `fit()` is available; using `fit_xy()` will
#'  generate an error. Second, the predictions will always be in a
#'  spark table format. The names will be the same as documented but
#'  without the dots. Third, there is no equivalent to factor
#'  columns in spark tables so class predictions are returned as
#'  character columns. Fourth, to retain the model object for a new
#'  R session (via `save()`), the `model$fit` element of the `parsnip`
#'  object should be serialized via `ml_save(object$fit)` and
#'  separately saved to disk. In a new session, the object can be
#'  reloaded and reattached to the `parsnip` object.
#'
#' @seealso [fit()]
#' @examples
#' show_engines("multinom_reg")
#'
#' multinom_reg()
#' # Parameters can be represented by a placeholder:
#' multinom_reg(penalty = varying())
#' @export
#' @importFrom purrr map_lgl
multinom_reg <-
  function(mode = "classification",
           penalty = NULL,
           mixture = NULL) {

    args <- list(
      penalty = enquo(penalty),
      mixture = enquo(mixture)
    )

    new_model_spec(
      "multinom_reg",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.multinom_reg <- function(x, ...) {
  cat("Multinomial Regression Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

#' @export
translate.multinom_reg <- translate.linear_reg

# ------------------------------------------------------------------------------

#' @inheritParams update.boost_tree
#' @param object A multinomial regression model specification.
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
           parameters = NULL,
           penalty = NULL, mixture = NULL,
           fresh = FALSE, ...) {

    eng_args <- update_engine_parameters(object$eng_args, ...)

    if (!is.null(parameters)) {
      parameters <- check_final_param(parameters)
    }
    args <- list(
      penalty = enquo(penalty),
      mixture = enquo(mixture)
    )

    args <- update_main_parameters(args, parameters)

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
      "multinom_reg",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

check_args.multinom_reg <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (all(is.numeric(args$penalty)) && any(args$penalty < 0))
    rlang::abort("The amount of regularization should be >= 0.")
  if (is.numeric(args$mixture) && (args$mixture < 0 | args$mixture > 1))
    rlang::abort("The mixture proportion should be within [0,1].")

  invisible(object)
}

# ------------------------------------------------------------------------------

#' @importFrom vctrs vec_size
organize_multnet_class <- function(x, object) {
  if (vec_size(x) > 1) {
    x <- x[,1]
  } else {
    x <- as.character(x)
  }
  x
}

#' @importFrom vctrs vec_size
organize_multnet_prob <- function(x, object) {
  x <- x[,,1]
  if (vec_size(x) > 1) {
    x <- as_tibble(x)
  } else {
    x <- tibble::as_tibble_row(x)
  }
  x
}

organize_nnet_prob <- function(x, object) {
  format_classprobs(x)
}




# ------------------------------------------------------------------------------
# glmnet call stack for multinomial regression using `predict` when object has
# classes "_multnet" and "model_fit" (for class predictions):
#
#  predict()
# 	predict._multnet(penalty = NULL)   <-- checks and sets penalty
#    predict.model_fit()               <-- checks for extra vars in ...
#     predict_class()
#      predict_class._multnet()
#       predict.multnet()


# glmnet call stack for multinomial regression using `multi_predict` when object has
# classes "_multnet" and "model_fit" (for class predictions):
#
# 	multi_predict()
#    multi_predict._multnet(penalty = NULL)
#      predict._multnet(multi = TRUE)          <-- checks and sets penalty
#       predict.model_fit()                    <-- checks for extra vars in ...
#        predict_raw()
#         predict_raw._multnet()
#          predict_raw.model_fit(opts = list(s = penalty))
#           predict.multnet()

# ------------------------------------------------------------------------------

#' @export
predict._multnet <-
  function(object, new_data, type = NULL, opts = list(), penalty = NULL, multi = FALSE, ...) {

    # See discussion in https://github.com/tidymodels/parsnip/issues/195
    if (is.null(penalty) & !is.null(object$spec$args$penalty)) {
      penalty <- object$spec$args$penalty
    }

    object$spec$args$penalty <- check_penalty(penalty, object, multi)

    object$spec <- eval_args(object$spec)
    res <- predict.model_fit(
      object = object,
      new_data = new_data,
      type = type,
      opts = opts
    )
    res
  }

#' @importFrom dplyr full_join as_tibble arrange
#' @importFrom tidyr gather
#' @export
#' @rdname multi_predict
multi_predict._multnet <-
  function(object, new_data, type = NULL, penalty = NULL, ...) {
    if (any(names(enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

    if (is_quosure(penalty))
      penalty <- eval_tidy(penalty)

    dots <- list(...)
    if (is.null(penalty)) {
      # See discussion in https://github.com/tidymodels/parsnip/issues/195
      if (!is.null(object$spec$args$penalty)) {
        penalty <- object$spec$args$penalty
      } else {
        penalty <- object$fit$lambda
      }
    }
    dots$s <- penalty

    if (is.null(type))
      type <- "class"
    if (!(type %in% c("class", "prob", "link", "raw"))) {
      rlang::abort("`type` should be either 'class', 'link', 'raw', or 'prob'.")
    }
    if (type == "prob")
      dots$type <- "response"
    else
      dots$type <- type

    object$spec <- eval_args(object$spec)
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
          .pred_class = factor(as.vector(pred), levels = object$lvl),
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

#' @export
predict_class._multnet <- function(object, new_data, ...) {
  object$spec <- eval_args(object$spec)
  predict_class.model_fit(object, new_data = new_data, ...)
}

#' @export
predict_classprob._multnet <- function(object, new_data, ...) {
  object$spec <- eval_args(object$spec)
  predict_classprob.model_fit(object, new_data = new_data, ...)
}

#' @export
predict_raw._multnet <- function(object, new_data, opts = list(), ...) {
  object$spec <- eval_args(object$spec)
  predict_raw.model_fit(object, new_data = new_data, opts = opts, ...)
}



# ------------------------------------------------------------------------------

# This checks as a pre-processor in the model data object
check_glmnet_lambda <- function(dat, object) {
  if (length(object$fit$lambda) > 1)
    rlang::abort(
      glue::glue(
      "`predict()` doesn't work with multiple penalties (i.e. lambdas). ",
      "Please specify a single value using `penalty = some_value` or use ",
      "`multi_predict()` to get multiple predictions per row of data."
      )
    )
  dat
}
