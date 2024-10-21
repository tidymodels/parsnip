#' @keywords internal
#' @rdname other_predict
#' @param quantile,quantile_levels  A vector of values between 0 and 1 for the
#' quantile to be predicted. If the model has a `"quantile regression"` mode,
#' this value should be `NULL`. For other modes, the default is `(1:9)/10`.
#' Note that, as of version 1.3.0 of parsnip, the `quantile` is deprecated. Use
#' `quantile_levels` instead.
#' @inheritParams predict.model_fit
#' @method predict_quantile model_fit
#' @export predict_quantile.model_fit
#' @export
predict_quantile.model_fit <- function(object,
                                       new_data,
                                       quantile_levels = NULL,
                                       quantile = deprecated(),
                                       interval = "none",
                                       level = 0.95,
                                       ...) {
  check_dots_empty()
  check_spec_pred_type(object, "quantile")

  if (lifecycle::is_present(quantile)) {
    lifecycle::deprecate_warn(
      "1.3.0",
      "predict_quantile(quantile)",
      "predict_quantile(quantile_levels)"
    )
    quantile_levels <- quantile
  }


  if (inherits(object$fit, "try-error")) {
    cli::cli_warn("Model fit failed; cannot make predictions.")
    return(NULL)
  }

  if (object$spec$mode == "quantile regression") {
    if (!is.null(quantile_levels)) {
      cli::cli_abort("When the mode is {.val quantile regression},
                     {.arg quantile_levels} are specified by {.fn set_mode}.",
                     call = rlang::call2("predict"))
    }
  } else {
    if (is.null(quantile_levels)) {
      quantile_levels <- (1:9)/10
    }
    hardhat::check_quantile_levels(quantile_levels)
    # Pass some extra arguments to be used in post-processor
    object$spec$quantile_levels <- quantile_levels
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
