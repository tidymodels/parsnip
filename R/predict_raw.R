# @rdname predict.model_fit
# @inheritParams predict.model_fit
# @method predict_raw model_fit
# @export predict_raw.model_fit
# @export
predict_raw.model_fit <- function(object, new_data, opts = list(), ...) {
  protected_args <- names(object$spec$method$pred$raw$args)
  dup_args <- names(opts) %in% protected_args
  if (any(dup_args)) {
    opts <- opts[[!dup_args]]
  }
  if (length(opts) > 0) {
    object$spec$method$pred$raw$args <-
      c(object$spec$method$pred$raw$args, opts)
  }

  if (!any(names(object$spec$method$pred) == "raw"))
    stop("No raw prediction module defined for this model.", call. = FALSE)

  if (inherits(object$fit, "try-error")) {
    rlang::warn("Model fit failed; cannot make predictions.")
    return(NULL)
  }

  new_data <- prepare_data(object, new_data)

  # preprocess data
  if (!is.null(object$spec$method$pred$raw$pre))
    new_data <- object$spec$method$pred$raw$pre(new_data, object)

  # create prediction call
  pred_call <- make_pred_call(object$spec$method$pred$raw)

  res <- eval_tidy(pred_call)

  res
}


# @export
# @rdname predict.model_fit
# @inheritParams predict_raw.model_fit
predict_raw <- function(object, ...)
  UseMethod("predict_raw")
