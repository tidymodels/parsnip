#' @keywords internal
#' @rdname other_predict
#' @inheritParams predict.model_fit
#' @method predict_time model_fit
#' @export predict_time.model_fit
#' @export
predict_time.model_fit <- function(object, new_data, ...) {
  if (object$spec$mode != "censored regression")
    rlang::abort(glue::glue("`predict_time()` is for predicting time outcomes. ",
                            "Use `predict_class()` or `predict_classprob()` for ",
                            "classification models."))

  if (!any(names(object$spec$method$pred) == "time"))
    rlang::abort("No prediction module defined for this model.")

  if (inherits(object$fit, "try-error")) {
    rlang::warn("Model fit failed; cannot make predictions.")
    return(NULL)
  }

  new_data <- prepare_data(object, new_data)

  # preprocess data
  if (!is.null(object$spec$method$pred$time$pre))
    new_data <- object$spec$method$pred$time$pre(new_data, object)

  # create prediction call
  pred_call <- make_pred_call(object$spec$method$pred$time)

  res <- eval_tidy(pred_call)
  # post-process the predictions

  if (!is.null(object$spec$method$pred$time$post)) {
    res <- object$spec$method$pred$time$post(res, object)
  }

  if (is.vector(res)) {
    res <- unname(res)
  } else {
    if (!inherits(res, "tbl_spark"))
      res <- as.data.frame(res)
  }
  res
}


#' @export
#' @keywords internal
#' @rdname other_predict
#' @inheritParams predict_time.model_fit
predict_time <- function(object, ...)
  UseMethod("predict_time")
