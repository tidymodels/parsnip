#' @keywords internal
#' @rdname other_predict
#' @inheritParams predict.model_fit
#' @method predict_survival model_fit
#' @export predict_survival.model_fit
#' @export
predict_survival.model_fit <-
  function(object, new_data, .time, ...) {
    if (object$spec$mode != "censored regression")
      rlang::abort(glue::glue(
        "`predict_survival()` is for predicting survival probabilities. ",
        "Use `predict_class()` or `predict_classprob()` for ",
        "classification models."
        ))

    if (is.null(object$spec$method$pred$survival))
      rlang::abort("No survival prediction method defined for this engine.")

    if (inherits(object$fit, "try-error")) {
      rlang::warn("Model fit failed; cannot make predictions.")
      return(NULL)
    }

    new_data <- prepare_data(object, new_data)

    # preprocess data
    if (!is.null(object$spec$method$pred$survival$pre))
      new_data <- object$spec$method$pred$survival$pre(new_data, object)

    # Pass some extra arguments to be used in post-processor
    object$spec$method$pred$survival$args$.time <- .time
    pred_call <- make_pred_call(object$spec$method$pred$survival)

    res <- eval_tidy(pred_call)

    # post-process the predictions
    if(!is.null(object$spec$method$pred$survival$post)) {
      res <- object$spec$method$pred$survival$post(res, object)
    }

    res
  }

# @export
# @keywords internal
# @rdname other_predict
# @inheritParams predict.model_fit
predict_survival <- function (object, ...)
  UseMethod("predict_survival")
