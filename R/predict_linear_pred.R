#' @keywords internal
#' @rdname other_predict
#' @inheritParams predict.model_fit
#' @method predict_linear_pred model_fit
#' @export predict_linear_pred.model_fit
#' @export
predict_linear_pred.model_fit <- function(object, new_data, ...) {

  check_spec_pred_type(object, "linear_pred")

  if (inherits(object$fit, "try-error")) {
    cli::cli_warn("Model fit failed; cannot make predictions.")
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
  }

  res
}


#' @export
#' @keywords internal
#' @rdname other_predict
#' @inheritParams predict_linear_pred.model_fit
predict_linear_pred <- function(object, ...)
  UseMethod("predict_linear_pred")
