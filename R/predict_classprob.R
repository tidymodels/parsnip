#' @keywords internal
#' @rdname other_predict
#' @inheritParams predict.model_fit
#' @method predict_classprob model_fit
#' @export predict_classprob.model_fit
#' @export
predict_classprob.model_fit <- function(object, new_data, ...) {
  if (object$spec$mode != "classification")
    rlang::abort("`predict.model_fit()` is for predicting factor outcomes.")

  check_spec_pred_type(object, "prob")
  check_spec_levels(object)

  if (inherits(object$fit, "try-error")) {
    rlang::warn("Model fit failed; cannot make predictions.")
    return(NULL)
  }

  new_data <- prepare_data(object, new_data)

  # preprocess data
  if (!is.null(object$spec$method$pred$prob$pre))
    new_data <- object$spec$method$pred$prob$pre(new_data, object)

  # create prediction call
  pred_call <- make_pred_call(object$spec$method$pred$prob)

  res <- eval_tidy(pred_call)

  # post-process the predictions
  if (!is.null(object$spec$method$pred$prob$post)) {
    res <- object$spec$method$pred$prob$post(res, object)
  }

  # check and sort names
  if (!is.data.frame(res) & !inherits(res, "tbl_spark"))
    rlang::abort("The was a problem with the probability predictions.")

  if (!is_tibble(res) & !inherits(res, "tbl_spark"))
    res <- as_tibble(res)

  res
}

# @export
# @keywords internal
# @rdname other_predict
# @inheritParams predict.model_fit
predict_classprob <- function(object, ...)
  UseMethod("predict_classprob")

check_spec_levels <- function(spec) {
  if ("class" %in% spec$lvl) {
    rlang::abort(
      glue::glue(
        "The outcome variable `{spec$preproc$y_var}` has a level called 'class'. ",
        "This level is reserved for parsnip's classification internals; please ",
        "adjust the levels to use a different value."
      ),
      call = NULL
    )
  }
}
