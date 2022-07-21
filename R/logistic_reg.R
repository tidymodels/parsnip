#' Logistic regression
#'
#' @description
#' [logistic_reg()] defines a generalized linear model for binary outcomes. A
#' linear combination of the predictors is used to model the log odds of an
#' event. This function can fit classification models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("logistic_reg")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "classification".
#' @param engine A single character string specifying what computational engine
#'  to use for fitting. Possible engines are listed below. The default for this
#'  model is `"glm"`.
#' @param penalty A non-negative number representing the total
#'  amount of regularization (specific engines only).
#'  For `keras` models, this corresponds to purely L2 regularization
#'  (aka weight decay) while the other models can be either or a combination
#'  of L1 and L2 (depending on the value of `mixture`).
#' @param mixture A number between zero and one (inclusive) giving the
#'  proportion of L1 regularization (i.e. lasso) in the model.
#'
#'  * `mixture = 1` specifies a pure lasso model,
#'  * `mixture = 0`  specifies a ridge regression model, and
#'  * `0 < mixture < 1` specifies an elastic net model, interpolating lasso and ridge.
#'
#'  Available for specific engines only. For `LiblineaR` models, `mixture` must
#'  be exactly 1 or 0 only.
#'
#' @template spec-details
#'
#' @details This model fits a classification model for binary outcomes; for
#' multiclass outcomes, see [multinom_reg()].
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("logistic_reg")}
#'
#' @examples
#' show_engines("logistic_reg")
#'
#' logistic_reg()
#' @export
logistic_reg <-
  function(mode = "classification",
           engine = "glm",
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
      engine = engine
    )
  }

#' @export
translate.logistic_reg <- function(x, engine = x$engine, ...) {
  x <- translate.default(x, engine, ...)

  # slightly cleaner code using
  arg_vals <- x$method$fit$args
  arg_names <- names(arg_vals)

  if (engine == "glmnet") {
    .check_glmnet_penalty_fit(x)
    if (any(names(x$eng_args) == "path_values")) {
      # Since we decouple the parsnip `penalty` argument from being the same
      # as the glmnet `lambda` value, `path_values` allows users to set the
      # path differently from the default that glmnet uses. See
      # https://github.com/tidymodels/parsnip/issues/431
      x$method$fit$args$lambda <- x$eng_args$path_values
      x$eng_args$path_values <- NULL
      x$method$fit$args$path_values <- NULL
    } else {
      # See discussion in https://github.com/tidymodels/parsnip/issues/195
      x$method$fit$args$lambda <- NULL
    }
    # Since the `fit` information is gone for the penalty, we need to have an
    # evaluated value for the parameter.
    x$args$penalty <- rlang::eval_tidy(x$args$penalty)
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
    x$method$fit$args <- arg_vals
  }
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

    args <- list(
      penalty = enquo(penalty),
      mixture = enquo(mixture)
    )

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "logistic_reg",
      ...
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

  object$spec$args$penalty <- .check_glmnet_penalty_predict(penalty, object, multi)

  object$spec <- eval_args(object$spec)
  predict.model_fit(object, new_data = new_data, type = type, opts = opts, ...)
}


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
    if (utils::packageVersion("dplyr") >= "1.0.99.9000") {
      pred <- full_join(param_key, pred, by = "group", multiple = "all")
    } else {
      pred <- full_join(param_key, pred, by = "group")
    }
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
