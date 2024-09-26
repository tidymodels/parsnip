#' @keywords internal
#' @rdname other_predict
#' @param quantile_levels  A vector of values between zero and one for the
#' quantile to be predicted. If the model has a `"censored regression"` mode,
#' this value should be `NULL`. For other modes, the default is `(1:9)/10`.
#' @inheritParams predict.model_fit
#' @method predict_quantile model_fit
#' @export predict_quantile.model_fit
#' @export
predict_quantile.model_fit <- function(object,
                                       new_data,
                                       quantile_levels = NULL,
                                       interval = "none",
                                       level = 0.95,
                                       ...) {

  check_spec_pred_type(object, "quantile")

  if (inherits(object$fit, "try-error")) {
    cli::cli_warn("Model fit failed; cannot make predictions.")
    return(NULL)
  }

  if (object$spec$mode == "quantile regression") {
    if (!is.null(quantile_levels)) {
      cli::cli_abort("When the mode is {.val quantile regression},
                     {.arg quantile_levels} are specified by {.fn set_mode}.")
    }
  } else {
    if (is.null(quantile_levels)) {
      quantile_levels <- (1:9)/10
    }
    hardhat::check_quantile_levels(quantile_levels)
    # Pass some extra arguments to be used in post-processor
    object$quantile_levels <- quantile_levels
  }

  new_data <- prepare_data(object, new_data)

  # preprocess data
  if (!is.null(object$spec$method$pred$quantile$pre)) {
    new_data <- object$spec$method$pred$quantile$pre(new_data, object)
  }

  pred_call <- make_pred_call(object$spec$method$pred$quantile)

  res <- eval_tidy(pred_call)

  # post-process the predictions
  if(!is.null(object$spec$method$pred$quantile$post)) {
    res <- object$spec$method$pred$quantile$post(res, object)
  }

  res
}

# @export
# @keywords internal
# @rdname other_predict
# @inheritParams predict.model_fit
predict_quantile <- function (object, ...) {
  UseMethod("predict_quantile")
}
