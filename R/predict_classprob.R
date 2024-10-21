#' @keywords internal
#' @rdname other_predict
#' @inheritParams predict.model_fit
#' @method predict_classprob model_fit
#' @export predict_classprob.model_fit
#' @export
predict_classprob.model_fit <- function(object, new_data, ...) {
  if (object$spec$mode != "classification") {
    cli::cli_abort("{.fun predict.model_fit()} is for predicting factor outcomes.",
                   call = rlang::call2("predict"))
  }

  check_spec_pred_type(object, "prob", call = caller_env())
  check_spec_levels(object)

  if (inherits(object$fit, "try-error")) {
    cli::cli_warn("Model fit failed; cannot make predictions.")
    return(NULL)
  }

  new_data <- prepare_data(object, new_data)

  # preprocess data
  if (!is.null(object$spec$method$pred$prob$pre)) {
    new_data <- object$spec$method$pred$prob$pre(new_data, object)
  }

  # create prediction call
  pred_call <- make_pred_call(object$spec$method$pred$prob)

  res <- eval_tidy(pred_call)

  # post-process the predictions
  if (!is.null(object$spec$method$pred$prob$post)) {
    res <- object$spec$method$pred$prob$post(res, object)
  }

  # check and sort names
  if (!is.data.frame(res) & !inherits(res, "tbl_spark")) {
    cli::cli_abort("The was a problem with the probability predictions.",
                   call = rlang::call2("predict"))
  }

  if (!is_tibble(res) & !inherits(res, "tbl_spark")) {
    res <- as_tibble(res)
  }

  res
}

# @export
# @keywords internal
# @rdname other_predict
# @inheritParams predict.model_fit
predict_classprob <- function(object, ...) {
  UseMethod("predict_classprob")
}

check_spec_levels <- function(spec) {
  if ("class" %in% spec$lvl) {
    cli::cli_abort(
      c(
        "The outcome variable {.var {spec$preproc$y_var}} has a level called {.val class}.",
        "i" = "This value is reserved for parsnip's classification internals; please
               change the levels, perhaps with {.fn forcats::fct_relevel}.",
        call = NULL
      )
    )
  }
}
