#' @keywords internal
#' @rdname other_predict
#' @inheritParams predict.model_fit
#' @method predict_survival model_fit
#' @export predict_survival.model_fit
#' @export
predict_survival.model_fit <- function(
  object,
  new_data,
  eval_time,
  time = deprecated(),
  interval = "none",
  level = 0.95,
  add_censoring_weights = FALSE,
  ...
) {
  if (lifecycle::is_present(time)) {
    lifecycle::deprecate_warn(
      "1.0.5",
      "predict_survival(time)",
      "predict_survival(eval_time)"
    )
    eval_time <- time
  }
  eval_time <- .filter_eval_time(eval_time)

  check_spec_pred_type(object, "survival")
  check_bool(add_censoring_weights)

  if (inherits(object$fit, "try-error")) {
    cli::cli_warn("Model fit failed; cannot make predictions.")
    return(NULL)
  }

  if (add_censoring_weights) {
    y_col <- .find_surv_col(new_data, fail = FALSE)
    if (length(y_col) == 0) {
      cli::cli_abort(
        c(
          "{.code add_censoring_weights = TRUE} requires a {.cls Surv} 
           column in {.arg new_data}."
        )
      )
    }
    surv_outcome <- new_data[[y_col]]
  }

  new_data <- prepare_data(object, new_data)

  # preprocess data
  if (!is.null(object$spec$method$pred$survival$pre)) {
    new_data <- object$spec$method$pred$survival$pre(new_data, object)
  }

  # create prediction call
  pred_call <- make_pred_call(object$spec$method$pred$survival)

  res <- eval_tidy(pred_call)

  # post-process the predictions
  if (!is.null(object$spec$method$pred$survival$post)) {
    res <- object$spec$method$pred$survival$post(res, object)
  }

  if (add_censoring_weights) {
    res[[y_col]] <- surv_outcome
    res <- .censoring_weights_graf(object, res)
    res[[y_col]] <- NULL
  }

  res
}

#' @export
#' @keywords internal
#' @rdname other_predict
#' @inheritParams predict_survival.model_fit
predict_survival <- function(object, ...) {
  UseMethod("predict_survival")
}
