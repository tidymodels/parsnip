#' @keywords internal
#' @rdname other_predict
#' @inheritParams predict.model_fit
#' @method predict_classprob model_fit
#' @export predict_classprob.model_fit
#' @export
#' @importFrom tibble as_tibble is_tibble tibble
predict_classprob.model_fit <- function (object, new_data, ...) {
  if(object$spec$mode != "classification")
    stop("`predict.model_fit` is for predicting factor outcomes.",
         call. = FALSE)

  if (!any(names(object$spec$method) == "classprob"))
    stop("No class probability module defined for this model.", call. = FALSE)

  new_data <- prepare_data(object, new_data)

  # preprocess data
  if (!is.null(object$spec$method$classprob$pre))
    new_data <- object$spec$method$classprob$pre(new_data, object)

  # create prediction call
  pred_call <- make_pred_call(object$spec$method$classprob)

  res <- eval_tidy(pred_call)

  # post-process the predictions
  if(!is.null(object$spec$method$classprob$post)) {
    res <- object$spec$method$classprob$post(res, object)
  }

  # check and sort names
  if (!is.data.frame(res) & !inherits(res, "tbl_spark"))
    stop("The was a problem with the probability predictions.", call. = FALSE)

  if (!is_tibble(res) & !inherits(res, "tbl_spark"))
    res <- as_tibble(res)

  res
}

#' @export
#' @keywords internal
#' @rdname other_predict
#' @inheritParams predict.model_fit
predict_classprob <- function (object, ...)
  UseMethod("predict_classprob")
