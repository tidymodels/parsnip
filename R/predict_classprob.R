#' Classification Model Probability Predictions
#'
#' Apply a model to create class probability predictions when the outcome is
#' qualitative.
#'
#' @param object An object of class `model_fit`
#' @param newdata A rectangular data object, such as a data frame.
#' @param ... Arguments to pass to other methods (not currently used).
#' @return A tibble with columns for each class.
#' @method predict_classprob model_fit
#' @export predict_classprob.model_fit
#' @export
#' @importFrom tibble as_tibble is_tibble tibble
predict_classprob.model_fit <- function (object, newdata, ...) {
  if(object$spec$mode != "classification")
    stop("`predict.model_fit` is for predicting factor outcomes.",
         call. = FALSE)
  
  if (!any(names(object$spec$method) == "prob"))
    stop("No class probability module defined for this model.", call. = FALSE)
  
  newdata <- prepare_data(object, newdata)
  
  # preprocess data
  if (!is.null(object$spec$method$prob$pre))
    newdata <- object$spec$method$prob$pre(newdata, object)
  
  # create prediction call
  pred_call <- make_pred_call(object$spec$method$prob)
  
  res <- eval_tidy(pred_call)
  
  # post-process the predictions
  if(!is.null(object$spec$method$prob$post)) {
    res <- object$spec$method$prob$post(res, object)
  }
  
  # check and sort names
  if (!is.data.frame(res))
    stop("The was a problem with the probability predictions.", call. = FALSE)
  
  # if (!isTRUE(all(sort(colnames(res)) == sort(object$lvl))))
  #   stop("The was a problem with the probability predictions.", call. = FALSE)
  if (is_tibble(res))
    res <- as_tibble(res)
  
  # res <- res[, object$lvl]

  res
}

#' @export
#' @rdname predict_classprob.model_fit
predict_classprob <- function (object, ...)
  UseMethod("predict_classprob")
