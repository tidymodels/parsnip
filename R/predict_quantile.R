#' @keywords internal
#' @rdname other_predict
#' @param quant A vector of numbers between 0 and 1 for the quantile being 
#'  predicted. 
#' @inheritParams predict.model_fit
#' @method predict_quantile model_fit
#' @export predict_quantile.model_fit
#' @export
predict_quantile.model_fit <-
  function (object, new_data, quantile = (1:9)/10, ...) {
    
    if (is.null(object$spec$method$quantile))
      stop("No quantile prediction method defined for this ",
           "engine.", call. = FALSE)
    
    new_data <- prepare_data(object, new_data)
    
    # preprocess data
    if (!is.null(object$spec$method$quantile$pre))
      new_data <- object$spec$method$quantile$pre(new_data, object)
    
    # Pass some extra arguments to be used in post-processor
    object$spec$method$quantile$args$p <- quantile
    pred_call <- make_pred_call(object$spec$method$quantile)
    
    res <- eval_tidy(pred_call)
    
    # post-process the predictions
    if(!is.null(object$spec$method$quantile$post)) {
      res <- object$spec$method$quantile$post(res, object)
    }
    
    res
  }

#' @export
#' @keywords internal
#' @rdname other_predict
#' @inheritParams predict.model_fit
predict_quantile <- function (object, ...)
  UseMethod("predict_quantile")
