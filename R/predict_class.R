#' Classification Model Predictions
#' 
#' Apply a model to create predictions when the outcome is 
#' qualitative. 
#' 
#' @param object An object of class `model_fit`
#' @param newdata A rectangular data object, such as a data frame.
#' @param ... Arguments to pass to other methods (not currently used).
#' @return An unnamed factor vector of predictions with the same
#' levels as the original outcome.  
#' @method predict_class model_fit
#' @export predict_class.model_fit
#' @export
predict_class.model_fit <- function (object, newdata, ...) {
  if(object$spec$mode != "classification")
    stop("`predict.model_fit` is for predicting factor outcomes.",
         call. = FALSE)
  fit_interface <- object$spec$method$fit$interface
  
  if(!is.na(object$preproc)) {
    # Translation code
    if(fit_interface == "formula") {
      newdata <- parsnip:::convert_xy_to_form_new(object$preproc, newdata)
    } else {
      newdata <- parsnip:::convert_xy_to_form_new(object$preproc, newdata)
    }
  }
  
  # preprocess data
  if(!is.null(object$spec$method$pred$pre)) {
    newdata <- object$spec$method$pred$pre(newdata, object)
  }
  
  # create prediction call
  pred_call <- make_pred_call(object$spec$method$pred)
  
  res <- eval_tidy(pred_call)
  # post-process the predictions
  
  if(!is.null(object$spec$method$pred$post)) {
    res <- object$spec$method$pred$post(res, object)
  }
  
  res
}

#' @export
#' @rdname predict_class.model_fit
predict_class <- function (object, ...) 
  UseMethod("predict_class")
