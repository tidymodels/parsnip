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
predict_class.model_fit <- function (object, new_data, ...) {
  if(object$spec$mode != "classification")
    stop("`predict.model_fit` is for predicting factor outcomes.",
         call. = FALSE)

  if (!any(names(object$spec$method) == "classes"))
    stop("No class prediction module defined for this model.", call. = FALSE)

  new_data <- prepare_data(object, new_data)

  # preprocess data
  if (!is.null(object$spec$method$classes$pre))
    new_data <- object$spec$method$classes$pre(new_data, object)

  # create prediction call
  pred_call <- make_pred_call(object$spec$method$classes)

  res <- eval_tidy(pred_call)

  # post-process the predictions
  if(!is.null(object$spec$method$classes$post)) {
    res <- object$spec$method$classes$post(res, object)
  }

  # coerce levels to those in `object`
  if (is.vector(res) || is.factor(res)) {
    res <- factor(as.character(res), levels = object$lvl)
  } else {
    if (!inherits(res, "tbl_spark"))
      res$values <- factor(as.character(res$values), levels = object$lvl)
  }

  res
}

#' @export
#' @keywords internal
#' @rdname other_predict
#' @inheritParams predict.model_fit
predict_class <- function (object, ...)
  UseMethod("predict_class")
