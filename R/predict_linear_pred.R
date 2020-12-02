#' @keywords internal
#' @rdname other_predict
#' @inheritParams predict.model_fit
#' @method predict_linear_pred model_fit
#' @export predict_linear_pred.model_fit
#' @export
predict_linear_pred.model_fit <- function(object, new_data, ...) {

  if (!any(names(object$spec$method$pred) == "linear_pred"))
    rlang::abort("No prediction module defined for this model.")

  if (inherits(object$fit, "try-error")) {
    rlang::warn("Model fit failed; cannot make predictions.")
    return(NULL)
  }

  new_data <- prepare_data(object, new_data)

  # preprocess data
  if (!is.null(object$spec$method$pred$linear_pred$pre))
    new_data <- object$spec$method$pred$linear_pred$pre(new_data, object)

  # create prediction call
  pred_call <- make_pred_call(object$spec$method$pred$linear_pred)

  res <- eval_tidy(pred_call)
  # post-process the predictions

  if (!is.null(object$spec$method$pred$linear_pred$post)) {
    res <- object$spec$method$pred$linear_pred$post(res, object)
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
#' @inheritParams predict_linear_pred.model_fit
predict_linear_pred <- function(object, ...)
  UseMethod("predict_linear_pred")
