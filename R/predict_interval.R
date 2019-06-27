# @keywords internal
# @rdname other_predict
# @param level A single numeric value between zero and one for the
#  interval estimates.
# @param std_error A single logical for wether the standard error should be
#  returned (assuming that the model can compute it).
# @inheritParams predict.model_fit
# @method predict_confint model_fit
# @export predict_confint.model_fit
# @export
predict_confint.model_fit <- function(object, new_data, level = 0.95, std_error = FALSE, ...) {

  if (is.null(object$spec$method$pred$conf_int))
    stop("No confidence interval method defined for this ",
         "engine.", call. = FALSE)

  if (inherits(object$fit, "try-error")) {
    warning("Model fit failed; cannot make predictions.", call. = FALSE)
    return(NULL)
  }

  new_data <- prepare_data(object, new_data)

  # preprocess data
  if (!is.null(object$spec$method$pred$conf_int$pre))
    new_data <- object$spec$method$pred$conf_int$pre(new_data, object)

  # Pass some extra arguments to be used in post-processor
  object$spec$method$pred$conf_int$extras <-
    list(level = level, std_error = std_error)
  pred_call <- make_pred_call(object$spec$method$pred$conf_int)

  res <- eval_tidy(pred_call)

  # post-process the predictions
  if (!is.null(object$spec$method$pred$conf_int$post)) {
    res <- object$spec$method$pred$conf_int$post(res, object)
  }

  attr(res, "level") <- level

  res
}

# @export
# @keywords internal
# @rdname other_predict
# @inheritParams predict.model_fit
predict_confint <- function(object, ...)
  UseMethod("predict_confint")

# ------------------------------------------------------------------------------

# @keywords internal
# @rdname other_predict
# @inheritParams predict.model_fit
# @method predict_predint model_fit
# @export predict_predint.model_fit
# @export
predict_predint.model_fit <- function(object, new_data, level = 0.95, std_error = FALSE, ...) {

  if (is.null(object$spec$method$pred$pred_int))
    stop("No prediction interval method defined for this ",
         "engine.", call. = FALSE)

  if (inherits(object$fit, "try-error")) {
    warning("Model fit failed; cannot make predictions.", call. = FALSE)
    return(NULL)
  }

  new_data <- prepare_data(object, new_data)

  # preprocess data
  if (!is.null(object$spec$method$pred$pred_int$pre))
    new_data <- object$spec$method$pred$pred_int$pre(new_data, object)

  # create prediction call
  # Pass some extra arguments to be used in post-processor
  object$spec$method$pred$pred_int$extras <-
    list(level = level, std_error = std_error)
  pred_call <- make_pred_call(object$spec$method$pred$pred_int)

  res <- eval_tidy(pred_call)

  # post-process the predictions
  if (!is.null(object$spec$method$pred$pred_int$post)) {
    res <- object$spec$method$pred$pred_int$post(res, object)
  }

  attr(res, "level") <- level

  res
}

# @export
# @keywords internal
# @rdname other_predict
# @inheritParams predict.model_fit
predict_predint <- function(object, ...)
  UseMethod("predict_predint")

