#' Multinomial regression
#'
#' @description
#'
#' `multinom_reg()` defines a model that uses linear predictors to predict
#' multiclass data using the multinomial distribution. This function can fit
#' classification models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("multinom_reg")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "classification".
#' @param engine A single character string specifying what computational engine
#'  to use for fitting. Possible engines are listed below. The default for this
#'  model is `"nnet"`.
#' @param penalty A non-negative number representing the total
#'  amount of regularization (specific engines only).
#'  For `keras` models, this corresponds to purely L2 regularization
#'  (aka weight decay) while the other models can be a combination
#'  of L1 and L2 (depending on the value of `mixture`).
#' @param mixture A number between zero and one (inclusive) that is the
#'  proportion of L1 regularization (i.e. lasso) in the model. When
#'  `mixture = 1`, it is a pure lasso model while `mixture = 0` indicates that
#'  ridge regression is being used. (specific engines only).
#'
#' @template spec-details
#'
#' @details This model fits a classification model for multiclass outcomes; for
#' binary outcomes, see [logistic_reg()].
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("multinom_reg")}
#'
#' @examples
#' show_engines("multinom_reg")
#'
#' multinom_reg()
#' @export
multinom_reg <-
  function(mode = "classification",
           engine = "nnet",
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
      engine = engine
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

#' @method update multinom_reg
#' @rdname parsnip_update
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

organize_multnet_class <- function(x, object) {
  if (vec_size(x) > 1) {
    x <- x[,1]
  } else {
    x <- as.character(x)
  }
  x
}

organize_multnet_prob <- function(x, object) {
  if (vec_size(x) > 1) {
    x <- as_tibble(x[,,1])
  } else {
    x <- tibble::as_tibble_row(x[,,1])
  }
  x
}

organize_nnet_prob <- function(x, object) {
  if (is.null(nrow(x))) {
    x_names <- names(x)
    x <- matrix(x, nrow = 1)
    colnames(x) <- x_names
  }
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

    object$spec$args$penalty <- .check_glmnet_penalty_predict(penalty, object, multi)

    object$spec <- eval_args(object$spec)
    res <- predict.model_fit(
      object = object,
      new_data = new_data,
      type = type,
      opts = opts
    )
    res
  }

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
