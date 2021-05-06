#' General Interface for Logistic Regression Models
#'
#' `logistic_reg()` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R, Stan, keras, or via Spark. The main
#'  arguments for the model are:
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
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "classification".
#' @param penalty A non-negative number representing the total
#'  amount of regularization (`glmnet`, `LiblineaR`, `keras`, and `spark` only).
#'  For `keras` models, this corresponds to purely L2 regularization
#'  (aka weight decay) while the other models can be either or a combination
#'  of L1 and L2 (depending on the value of `mixture`).
#' @param mixture A number between zero and one (inclusive) that is the
#'  proportion of L1 regularization (i.e. lasso) in the model. When
#'  `mixture = 1`, it is a pure lasso model while `mixture = 0` indicates that
#'  ridge regression is being used. (`glmnet`, `LiblineaR`, and `spark` only).
#'  For `LiblineaR` models, `mixture` must be exactly 0 or 1 only.
#' @details
#' For `logistic_reg()`, the mode will always be "classification".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"glm"`  (the default), `"glmnet"`, or `"LiblineaR"`
#' \item \pkg{Stan}:  `"stan"`
#' \item \pkg{Spark}: `"spark"`
#' \item \pkg{keras}: `"keras"`
#' }
#'
#' For this model, other packages may add additional engines. Use
#' [show_engines()] to see the current set of engines.
#'
#' @includeRmd man/rmd/logistic-reg.Rmd details
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
#' @seealso [fit()], [set_engine()], [update()]
#' @examples
#' show_engines("logistic_reg")
#'
#' logistic_reg()
#' # Parameters can be represented by a placeholder:
#' logistic_reg(penalty = varying())
#' @export
#' @importFrom purrr map_lgl
logistic_reg <-
  function(mode = "classification",
           penalty = NULL,
           mixture = NULL) {

    args <- list(
      penalty = enquo(penalty),
      mixture = enquo(mixture)
    )

    new_model_spec(
      "logistic_reg",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
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

#' @export
translate.logistic_reg <- function(x, engine = x$engine, ...) {
  x <- translate.default(x, engine, ...)

  # slightly cleaner code using
  arg_vals <- x$method$fit$args
  arg_names <- names(arg_vals)


  if (engine == "glmnet") {
    # See discussion in https://github.com/tidymodels/parsnip/issues/195
    arg_vals$lambda <- NULL
    # Since the `fit` information is gone for the penalty, we need to have an
    # evaluated value for the parameter.
    x$args$penalty <- rlang::eval_tidy(x$args$penalty)
    if (length(x$args$penalty) != 1) {
      rlang::abort(c(
        "For the glmnet engine, `penalty` must be a single number.",
        glue::glue("There are {length(x$args$penalty)} values for `penalty`."),
        "To try multiple values for total regularization, use the tune package."
      ))
    }
  }

  if (engine == "LiblineaR") {
    # convert parameter arguments
    new_penalty <- rlang::eval_tidy(x$args$penalty)
    if (is.numeric(new_penalty))
      arg_vals$cost <- rlang::new_quosure(1 / new_penalty, env = rlang::empty_env())

    if (any(arg_names == "type")) {
      if (is.numeric(quo_get_expr(arg_vals$type)))
        if (quo_get_expr(x$args$mixture) == 0) {
          arg_vals$type <- 0      ## ridge
        } else if (quo_get_expr(x$args$mixture) == 1) {
          arg_vals$type <- 6      ## lasso
        } else {
          rlang::abort("For the LiblineaR engine, mixture must be 0 or 1.")
        }
    }

  }

  x$method$fit$args <- arg_vals

  x
}

# ------------------------------------------------------------------------------

#' @method update logistic_reg
#' @rdname parsnip_update
#' @export
update.logistic_reg <-
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
      "logistic_reg",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

check_args.logistic_reg <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (all(is.numeric(args$penalty)) && any(args$penalty < 0))
    rlang::abort("The amount of regularization should be >= 0.")
  if (is.numeric(args$mixture) && (args$mixture < 0 | args$mixture > 1))
    rlang::abort("The mixture proportion should be within [0,1].")
  if (is.numeric(args$mixture) && length(args$mixture) > 1)
    rlang::abort("Only one value of `mixture` is allowed.")

  if (object$engine == "LiblineaR") {
    if(is.numeric(args$mixture) && !args$mixture %in% 0:1)
      rlang::abort(c("For the LiblineaR engine, mixture must be 0 or 1.",
                     "Choose a pure ridge model with `mixture = 0`.",
                     "Choose a pure lasso model with `mixture = 1`.",
                     "The Liblinear engine does not support other values."))
    if(all(is.numeric(args$penalty)) && !all(args$penalty > 0))
      rlang::abort("For the LiblineaR engine, penalty must be > 0.")
  }

  invisible(object)
}

# ------------------------------------------------------------------------------

prob_to_class_2 <- function(x, object) {
  x <- ifelse(x >= 0.5, object$lvl[2], object$lvl[1])
  unname(x)
}


organize_glmnet_class <- function(x, object) {
  if (ncol(x) == 1) {
    res <- prob_to_class_2(x[, 1], object)
  } else {
    n <- nrow(x)
    res <- utils::stack(as.data.frame(x))
    res$values <- prob_to_class_2(res$values, object)
    if (!is.null(object$spec$args$penalty))
      res$lambda <- rep(object$spec$args$penalty, each = n) else
        res$lambda <- rep(object$fit$lambda, each = n)
    res <- res[, colnames(res) %in% c("values", "lambda")]
  }
  res
}

organize_glmnet_prob <- function(x, object) {
  if (ncol(x) == 1) {
    res <- tibble(v1 = 1 - x[, 1], v2 = x[, 1])
    colnames(res) <- object$lvl
  } else {
    n <- nrow(x)
    res <- utils::stack(as.data.frame(x))
    res <- tibble(v1 = 1 - res$values, v2 = res$values)
    colnames(res) <- object$lvl
    if (!is.null(object$spec$args$penalty))
      res$lambda <- rep(object$spec$args$penalty, each = n) else
        res$lambda <- rep(object$fit$lambda, each = n)
  }
  res
}

# ------------------------------------------------------------------------------
# glmnet call stack for logistic regression using `predict` when object has
# classes "_lognet" and "model_fit" (for class predictions):
#
#  predict()
# 	predict._lognet(penalty = NULL)    <-- checks and sets penalty
#    predict.model_fit()               <-- checks for extra vars in ...
#     predict_class()
#      predict_class._lognet()
#       predict_class.model_fit()
#        predict.lognet()


# glmnet call stack for logistic regression using `multi_predict` when object has
# classes "_lognet" and "model_fit" (for class predictions):
#
# 	multi_predict()
#    multi_predict._lognet(penalty = NULL)
#      predict._lognet(multi = TRUE)           <-- checks and sets penalty
#       predict.model_fit()                    <-- checks for extra vars in ...
#        predict_raw()
#         predict_raw._lognet()
#          predict_raw.model_fit(opts = list(s = penalty))
#           predict.lognet()

# ------------------------------------------------------------------------------

#' @export
predict._lognet <- function(object, new_data, type = NULL, opts = list(), penalty = NULL, multi = FALSE, ...) {
  if (any(names(enquos(...)) == "newdata"))
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

  # See discussion in https://github.com/tidymodels/parsnip/issues/195
  if (is.null(penalty) & !is.null(object$spec$args$penalty)) {
    penalty <- object$spec$args$penalty
  }

  object$spec$args$penalty <- check_penalty(penalty, object, multi)

  object$spec <- eval_args(object$spec)
  predict.model_fit(object, new_data = new_data, type = type, opts = opts, ...)
}


#' @importFrom dplyr full_join as_tibble arrange
#' @importFrom tidyr gather
#' @export
#' @rdname multi_predict
multi_predict._lognet <-
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
    param_key <- tibble(group = colnames(pred), penalty = penalty)
    pred <- as_tibble(pred)
    pred$.row <- 1:nrow(pred)
    pred <- gather(pred, group, .pred_class, -.row)
    if (dots$type == "class") {
      pred[[".pred_class"]] <- factor(pred[[".pred_class"]], levels = object$lvl)
    } else {
      if (dots$type == "response") {
        pred[[".pred2"]] <- 1 - pred[[".pred_class"]]
        names(pred) <- c(".row", "group", paste0(".pred_", rev(object$lvl)))
        pred <- pred[, c(".row", "group", paste0(".pred_", object$lvl))]
      }
    }
    pred <- full_join(param_key, pred, by = "group")
    pred$group <- NULL
    pred <- arrange(pred, .row, penalty)
    .row <- pred$.row
    pred$.row <- NULL
    pred <- split(pred, .row)
    names(pred) <- NULL
    tibble(.pred = pred)
  }





#' @export
predict_class._lognet <- function(object, new_data, ...) {
  if (any(names(enquos(...)) == "newdata"))
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

  object$spec <- eval_args(object$spec)
  predict_class.model_fit(object, new_data = new_data, ...)
}

#' @export
predict_classprob._lognet <- function(object, new_data, ...) {
  if (any(names(enquos(...)) == "newdata"))
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

  object$spec <- eval_args(object$spec)
  predict_classprob.model_fit(object, new_data = new_data, ...)
}

#' @export
predict_raw._lognet <- function(object, new_data, opts = list(), ...) {
  if (any(names(enquos(...)) == "newdata"))
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

  object$spec <- eval_args(object$spec)
  predict_raw.model_fit(object, new_data = new_data, opts = opts, ...)
}

# ------------------------------------------------------------------------------

liblinear_preds <- function(results, object) {
  results$predictions
}

liblinear_probs <- function(results, object) {
  as_tibble(results$probabilities)
}
