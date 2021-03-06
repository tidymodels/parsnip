#' @keywords internal
#' @rdname other_predict
#' @inheritParams predict.model_fit
#' @method predict_hazard model_fit
#' @export predict_hazard.model_fit
#' @export
predict_hazard.model_fit <-
  function(object, new_data, .time, ...) {

    if (is.null(object$spec$method$pred$hazard))
      rlang::abort("No hazard prediction method defined for this engine.")

    if (inherits(object$fit, "try-error")) {
      rlang::warn("Model fit failed; cannot make predictions.")
      return(NULL)
    }

    new_data <- prepare_data(object, new_data)

    # preprocess data
    if (!is.null(object$spec$method$pred$hazard$pre))
      new_data <- object$spec$method$pred$hazard$pre(new_data, object)

    # Pass some extra arguments to be used in post-processor
    object$spec$method$pred$hazard$args$.time <- .time
    pred_call <- make_pred_call(object$spec$method$pred$hazard)

    res <- eval_tidy(pred_call)

    # post-process the predictions
    if(!is.null(object$spec$method$pred$hazard$post)) {
      res <- object$spec$method$pred$hazard$post(res, object)
    }

    res
  }

# @export
# @keywords internal
# @rdname other_predict
# @inheritParams predict.model_fit
predict_hazard <- function (object, ...)
  UseMethod("predict_hazard")
