#' @keywords internal
#' @rdname other_predict
#' @inheritParams predict.model_fit
#' @method predict_time model_fit
#' @export predict_time.model_fit
#' @export
predict_time.model_fit <- function(object, new_data, ...) {
  if (object$spec$mode != "censored regression") {
    cli::cli_abort(
      c(
        "{.fun predict_time} is for predicting time outcomes.",
        "i" = "Use {.fun predict_class} or {.fun predict_classprob} for
               classification models."
      ),
      call = rlang::call2("predict")
    )
  }

  check_spec_pred_type(object, "time")

  if (inherits(object$fit, "try-error")) {
    cli::cli_warn("Model fit failed; cannot make predictions.")
    return(NULL)
  }

  new_data <- prepare_data(object, new_data)

  # preprocess data
  if (!is.null(object$spec$method$pred$time$pre)) {
    new_data <- object$spec$method$pred$time$pre(new_data, object)
  }

  # create prediction call
  pred_call <- make_pred_call(object$spec$method$pred$time)

  res <- eval_tidy(pred_call)

  # post-process the predictions
  if (!is.null(object$spec$method$pred$time$post)) {
    res <- object$spec$method$pred$time$post(res, object)
  }

  if (is.vector(res)) {
    res <- unname(res)
  }

  res
}


#' @export
#' @keywords internal
#' @rdname other_predict
#' @inheritParams predict_time.model_fit
predict_time <- function(object, ...) {
  UseMethod("predict_time")
}
