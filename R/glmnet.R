# glmnet call stack using `predict()` when object has
# classes "_<glmnet-class>" and "model_fit":
#
#  predict()
#   predict._<glmnet-class>(penalty = NULL)
#    predict_glmnet(penalty = NULL)             <-- checks and sets penalty
#     predict.model_fit()                       <-- checks for extra vars in ...
#      predict_numeric()
#       predict_numeric._<glmnet-class>()
#        predict_numeric_glmnet()
#         predict_numeric.model_fit()
#          predict.<glmnet-class>()


# glmnet call stack using `multi_predict` when object has
# classes "_<glmnet-class>" and "model_fit":
#
#  multi_predict()
#   multi_predict._<glmnet-class>(penalty = NULL)
#    predict._<glmnet-class>(multi = TRUE)
#     predict_glmnet(multi = TRUE)            <-- checks and sets penalty
#      predict.model_fit()                    <-- checks for extra vars in ...
#       predict_raw()
#        predict_raw._<glmnet-class>()
#         predict_raw_glmnet()
#          predict_raw.model_fit(opts = list(s = penalty))
#           predict.<glmnet-class>()


predict_glmnet <- function(object,
                           new_data,
                           type = NULL,
                           opts = list(),
                           penalty = NULL,
                           multi = FALSE,
                           ...) {
  # See discussion in https://github.com/tidymodels/parsnip/issues/195
  if (is.null(penalty) & !is.null(object$spec$args$penalty)) {
    penalty <- object$spec$args$penalty
  }

  object$spec$args$penalty <- .check_glmnet_penalty_predict(penalty, object, multi)

  object$spec <- eval_args(object$spec)
  predict.model_fit(object, new_data = new_data, type = type, opts = opts, ...)
}

predict_numeric_glmnet <- function(object, new_data, ...) {
  object$spec <- eval_args(object$spec)
  predict_numeric.model_fit(object, new_data = new_data, ...)
}

predict_class_glmnet <- function(object, new_data, ...) {
  object$spec <- eval_args(object$spec)
  predict_class.model_fit(object, new_data = new_data, ...)
}

predict_classprob_glmnet <- function(object, new_data, ...) {
  object$spec <- eval_args(object$spec)
  predict_classprob.model_fit(object, new_data = new_data, ...)
}

predict_raw_glmnet <- function(object, new_data, opts = list(), ...)  {
  object$spec <- eval_args(object$spec)

  opts$s <- object$spec$args$penalty

  predict_raw.model_fit(object, new_data = new_data, opts = opts, ...)
}

multi_predict_glmnet <- function(object,
                                 new_data,
                                 type = NULL,
                                 penalty = NULL,
                                 ...) {

  if (any(names(enquos(...)) == "newdata")) {
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")
  }

  if (object$spec$mode == "classification") {
    if (is_quosure(penalty)) {
      penalty <- eval_tidy(penalty)
    }
  }

  dots <- list(...)

  object$spec <- eval_args(object$spec)

  if (is.null(penalty)) {
    # See discussion in https://github.com/tidymodels/parsnip/issues/195
    if (!is.null(object$spec$args$penalty)) {
      penalty <- object$spec$args$penalty
    } else {
      penalty <- object$fit$lambda
    }
  }

  if (object$spec$mode == "classification") {
    if (is.null(type)) {
      type <- "class"
    }
    if (!(type %in% c("class", "prob", "link", "raw"))) {
      rlang::abort("`type` should be either 'class', 'link', 'raw', or 'prob'.")
    }
    if (type == "prob") {
      dots$type <- "response"
    } else {
      dots$type <- type
    }
  }

  pred <- predict(object, new_data = new_data, type = "raw",
                  opts = dots, penalty = penalty, multi = TRUE)

  model_type <- class(object$spec)[1]
  res <- switch(
    model_type,
    "linear_reg" = format_glmnet_multi_linear_reg(pred, penalty = penalty),
    "logistic_reg" = format_glmnet_multi_logistic_reg(pred,
                                                      penalty = penalty,
                                                      type = dots$type,
                                                      lvl = object$lvl),
    "multinom_reg" = format_glmnet_multi_multinom_reg(pred,
                                                      penalty = penalty,
                                                      type = type,
                                                      n_rows = nrow(new_data),
                                                      lvl = object$lvl)
  )

  res
}
