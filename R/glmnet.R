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

# translation of glmnet classes to parsnip models
# elnet ~ linear_reg
#
# glmnetfit: that's a catch-all class for glmnet models fitted with a base-R
#  family, thus can be any of linear_reg, logistic_reg, multinom_reg, poisson_reg

#' @export
predict._elnet <- predict_glmnet

#' @export
predict_numeric._elnet <- predict_numeric_glmnet

#' @export
predict_raw._elnet <- predict_raw_glmnet

#' @export
predict._glmnetfit <- predict_glmnet

#' @export
predict_numeric._glmnetfit <- predict_numeric_glmnet

#' @export
predict_class._glmnetfit <- predict_class_glmnet

#' @export
predict_classprob._glmnetfit <- predict_classprob_glmnet

#' @export
predict_raw._glmnetfit <- predict_raw_glmnet

#' Organize glmnet predictions
#'
#' This function is for developer use and organizes predictions from glmnet
#' models.
#'
#' @param x Predictions as returned by the `predict()` method for glmnet models.
#' @param object An object of class `model_fit`.
#'
#' @rdname glmnet_helpers_prediction
#' @keywords internal
#' @export
.organize_glmnet_pred <- function(x, object) {
  unname(x[, 1])
}

# -------------------------------------------------------------------------

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

  model_type <- class(object$spec)[1]

  if (object$spec$mode == "classification") {
    if (is.null(type)) {
      type <- "class"
    }
    if (!(type %in% c("class", "prob", "link", "raw"))) {
      rlang::abort("`type` should be either 'class', 'link', 'raw', or 'prob'.")
    }
    if (type == "prob" |
        model_type == "logistic_reg") {
      dots$type <- "response"
    } else {
      dots$type <- type
    }
  }

  pred <- predict(object, new_data = new_data, type = "raw",
                  opts = dots, penalty = penalty, multi = TRUE)


  res <- switch(
    model_type,
    "linear_reg" = format_glmnet_multi_linear_reg(pred, penalty = penalty),
    "logistic_reg" = format_glmnet_multi_logistic_reg(pred,
                                                      penalty = penalty,
                                                      type = type,
                                                      lvl = object$lvl),
    "multinom_reg" = format_glmnet_multi_multinom_reg(pred,
                                                      penalty = penalty,
                                                      type = type,
                                                      n_rows = nrow(new_data),
                                                      lvl = object$lvl)
  )

  res
}

#' @export
#' @rdname multi_predict
#' @param penalty A numeric vector of penalty values.
multi_predict._elnet <- multi_predict_glmnet

#' @export
multi_predict._glmnetfit <- multi_predict_glmnet

format_glmnet_multi_linear_reg <- function(pred, penalty) {
  param_key <- tibble(group = colnames(pred), penalty = penalty)
  pred <- as_tibble(pred)
  pred$.row <- 1:nrow(pred)
  pred <- gather(pred, group, .pred, -.row)
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

# -------------------------------------------------------------------------

#' Helper functions for checking the penalty of glmnet models
#'
#' @description
#' These functions are for developer use.
#'
#' `.check_glmnet_penalty_fit()` checks that the model specification for fitting a
#' glmnet model contains a single value.
#'
#' `.check_glmnet_penalty_predict()` checks that the penalty value used for prediction is valid.
#' If called by `predict()`, it needs to be a single value. Multiple values are
#' allowed for `multi_predict()`.
#'
#' @param x An object of class `model_spec`.
#' @rdname glmnet_helpers
#' @keywords internal
#' @export
.check_glmnet_penalty_fit <- function(x) {
  pen <- rlang::eval_tidy(x$args$penalty)

  if (length(pen) != 1) {
    rlang::abort(c(
      "For the glmnet engine, `penalty` must be a single number (or a value of `tune()`).",
      glue::glue("There are {length(pen)} values for `penalty`."),
      "To try multiple values for total regularization, use the tune package.",
      "To predict multiple penalties, use `multi_predict()`"
    ))
  }
}

#' @param penalty A penalty value to check.
#' @param object An object of class `model_fit`.
#' @param multi A logical indicating if multiple values are allowed.
#'
#' @rdname glmnet_helpers
#' @keywords internal
#' @export
.check_glmnet_penalty_predict <- function(penalty = NULL, object, multi = FALSE) {
  if (is.null(penalty)) {
    penalty <- object$fit$lambda
  }

  # when using `predict()`, allow for a single lambda
  if (!multi) {
    if (length(penalty) != 1) {
      rlang::abort(
        glue::glue(
          "`penalty` should be a single numeric value. `multi_predict()` ",
          "can be used to get multiple predictions per row of data.",
        )
      )
    }
  }

  if (length(object$fit$lambda) == 1 && penalty != object$fit$lambda) {
    rlang::abort(
      glue::glue(
        "The glmnet model was fit with a single penalty value of ",
        "{object$fit$lambda}. Predicting with a value of {penalty} ",
        "will give incorrect results from `glmnet()`."
      )
    )
  }

  penalty
}

set_glmnet_penalty_path <- function(x) {
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
  x
}

