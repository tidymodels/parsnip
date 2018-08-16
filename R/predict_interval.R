#' @rdname predict.model_fit
#' @param level A single numeric value between zero and one for the
#'  interval estimates. 
#' @inheritParams predict.model_fit
#' @method predict_confint model_fit
#' @export predict_confint.model_fit
#' @export
predict_confint.model_fit <- function (object, newdata, level = 0.95, ...) {
  if(object$spec$mode != "regression")
    stop("`predict_confint` is for numeric outcomes.",
         call. = FALSE)
  
  if (is.null(object$spec$method$confint))
    stop("No confidence interval method defined for this ",
         "engine.", call. = FALSE)
  
  newdata <- prepare_data(object, newdata)
  
  # preprocess data
  if (!is.null(object$spec$method$confint$pre))
    newdata <- object$spec$method$confint$pre(newdata, object)

  # create prediction call
  object$spec$method$confint$args$level <- level
  pred_call <- make_pred_call(object$spec$method$confint)
  
  res <- eval_tidy(pred_call)
  
  # post-process the predictions
  if(!is.null(object$spec$method$confint$post)) {
    res <- object$spec$method$confint$post(res, object)
  }
  
  attr(res, "level") <- level
  
  res
}

#' @export
#' @rdname predict.model_fit
#' @inheritParams predict.model_fit
predict_confint <- function (object, ...)
  UseMethod("predict_confint")

##################################################################

#' @rdname predict.model_fit
#' @inheritParams predict.model_fit
#' @method predict_predint model_fit
#' @export predict_predint.model_fit
#' @export
predict_predint.model_fit <- function (object, newdata, level = 0.95, ...) {
  if(object$spec$mode != "regression")
    stop("`predict_predint` is for numeric outcomes.",
         call. = FALSE)
  
  if (is.null(object$spec$method$predint))
    stop("No prediction interval method defined for this ",
         "engine.", call. = FALSE)
  
  newdata <- prepare_data(object, newdata)
  
  # preprocess data
  if (!is.null(object$spec$method$predint$pre))
    newdata <- object$spec$method$predint$pre(newdata, object)
  
  # create prediction call
  object$spec$method$predint$args$level <- level
  pred_call <- make_pred_call(object$spec$method$predint)
  
  res <- eval_tidy(pred_call)
  
  # post-process the predictions
  if(!is.null(object$spec$method$predint$post)) {
    res <- object$spec$method$predint$post(res, object)
  }
  
  attr(res, "level") <- level
  
  res
}

#' @export
#' @rdname predict.model_fit
#' @inheritParams predict.model_fit
predict_predint <- function (object, ...)
  UseMethod("predict_predint")





