#' Model Predictions
#'
#' Apply a model to create different types of predictions. `predict` is used
#' when the outcome is a simple number. For classification models,
#' `predict_class` and  `predict_classprob` can be used.
#'
#' @param object An object of class `model_fit`
#' @param newdata A rectangular data object, such as a data frame.
#' @param ... Arguments to pass to other methods (not currently used).
#' @return `predict` returns an unnamed numeric vector. `predict_class` returns
#'  a factor variable and `predict_classprob` generates a tibble with a column
#'  for each class level.
#' @importFrom stats predict
#' @method predict model_fit
#' @export predict.model_fit
#' @export
predict.model_fit <- function (object, newdata, ...) {
  if (object$spec$mode != "regression")
    stop("`predict.model_fit` is for predicting numeric outcomes.  ",
         "Use `predict_class` or `predict_prob` for ",
         "classification models.", call. = FALSE)

  if (!any(names(object$spec$method) == "pred"))
    stop("No prediction module defined for this model.", call. = FALSE)

  newdata <- prepare_data(object, newdata)

  # preprocess data
  if (!is.null(object$spec$method$pred$pre))
    newdata <- object$spec$method$pred$pre(newdata, object)

  # create prediction call
  pred_call <- make_pred_call(object$spec$method$pred)

  res <- eval_tidy(pred_call)
  # post-process the predictions

  if (!is.null(object$spec$method$pred$post)) {
    res <- object$spec$method$pred$post(res, object)
  }

  if (is.vector(res)) {
    res <- unname(res)
  } else
    res <- as.data.frame(res)
  res
}


make_pred_call <- function(x) {
  if ("pkg" %in% names(x$func))
    cl <-
      call2(x$func["fun"],!!!x$args, .ns = x$func["pkg"])
  else
    cl <-   call2(x$func["fun"],!!!x$args)

  cl
}

prepare_data <- function(object, newdata) {
  fit_interface <- object$spec$method$fit$interface

  if (!all(is.na(object$preproc))) {
    # Translation code
    if (fit_interface == "formula") {
      newdata <- convert_xy_to_form_new(object$preproc, newdata)
    } else {
      newdata <- convert_form_to_xy_new(object$preproc, newdata)$x
    }
  }

  newdata
}

