#' Other predict methods.
#'
#' These are internal functions not meant to be directly called by the user.
#'
#' @keywords internal
#' @rdname other_predict
#' @inheritParams predict.model_fit
#' @method predict_class model_fit
#' @export predict_class.model_fit
#' @export
predict_class.model_fit <- function(object, new_data, ...) {
  if (object$spec$mode != "classification")
    rlang::abort("`predict.model_fit()` is for predicting factor outcomes.")

  check_spec_pred_type(object, "class")

  if (inherits(object$fit, "try-error")) {
    rlang::warn("Model fit failed; cannot make predictions.")
    return(NULL)
  }

  new_data <- prepare_data(object, new_data)

  # preprocess data
  if (!is.null(object$spec$method$pred$class$pre))
    new_data <- object$spec$method$pred$class$pre(new_data, object)

  # create prediction call
  pred_call <- make_pred_call(object$spec$method$pred$class)

  res <- eval_tidy(pred_call)

  # post-process the predictions
  if (!is.null(object$spec$method$pred$class$post)) {
    res <- object$spec$method$pred$class$post(res, object)
  }

  # coerce levels to those in `object`
  if (is.vector(res) || is.factor(res)) {
    res <- factor(as.character(res), levels = object$lvl)
  } else {
    if (!inherits(res, "tbl_spark")) {
      # Now case where a parsnip model generated `res`
      if (is.data.frame(res) && ncol(res) == 1 && is.factor(res[[1]])) {
        res <- res[[1]]
      } else {
        res$values <- factor(as.character(res$values), levels = object$lvl)
      }
    }
  }

  res
}

# @export
# @keywords internal
# @rdname other_predict
# @inheritParams predict.model_fit
predict_class <- function(object, ...)
  UseMethod("predict_class")

