# @keywords internal
# @rdname other_predict
# @inheritParams predict.model_fit
# @method predict_numeric model_fit
# @export predict_numeric.model_fit
# @export

predict_numeric.model_fit <- function(object, new_data, ...) {
  if (object$spec$mode != "regression")
    stop("`predict_numeric()` is for predicting numeric outcomes.  ",
         "Use `predict_class()` or `predict_classprob()` for ",
         "classification models.", call. = FALSE)

  if (!any(names(object$spec$method$pred) == "numeric"))
    stop("No prediction module defined for this model.", call. = FALSE)

  if (inherits(object$fit, "try-error")) {
    warning("Model fit failed; cannot make predictions.", call. = FALSE)
    return(NULL)
  }

  new_data <- prepare_data(object, new_data)

  # preprocess data
  if (!is.null(object$spec$method$pred$numeric$pre))
    new_data <- object$spec$method$pred$numeric$pre(new_data, object)

  # create prediction call
  pred_call <- make_pred_call(object$spec$method$pred$numeric)

  res <- eval_tidy(pred_call)
  # post-process the predictions

  if (!is.null(object$spec$method$pred$numeric$post)) {
    res <- object$spec$method$pred$numeric$post(res, object)
  }

  if (is.vector(res)) {
    res <- unname(res)
  } else {
    if (!inherits(res, "tbl_spark"))
      res <- as.data.frame(res)
  }
  res
}


# @export
# @keywords internal
# @rdname other_predict
# @inheritParams predict_numeric.model_fit
predict_numeric <- function(object, ...)
  UseMethod("predict_numeric")
