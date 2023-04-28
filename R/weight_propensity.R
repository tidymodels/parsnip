#' Helper for bridging two-stage causal fits
#'
#' @description
#' `weight_propensity()` is a helper function to more easily link the
#' propensity and outcome models in causal workflows. **The main documentation
#' for this function lives in the tune package at** `?tune::weight_propensity`.
#'
#' @param object The object containing the model fit(s) that will generate
#' predictions used to calculate propensity weights. Currently, either a
#' [parsnip model fit][parsnip::fit.model_spec()], fitted
#' [workflow][workflows::workflow()], or
#' tuning results (`?tune::fit_resamples`) object. If a tuning result, the
#' object must have been generated with the control argument
#' (`?tune::control_resamples`) `extract = identity`.
#' @param wt_fn A function used to calculate the propensity weights. The first
#' argument gives the predicted probability of exposure, the true value for
#' which is provided in the second argument. See `?propensity::wt_ate()` for
#' an example.
#' @param .treated The level of the exposure corresponding to the treatment, as
#' a string. Additionally passed as `.treated` to `wt_fn`.
#' @param ... Additional arguments passed to `wt_fn`.
#' @param data The data supplied as the `data` argument to `fit()` the `object`.
#' This argument is only required for the `model_fit` and `workflow` methods---the
#' needed data for the `tune_results` method lives inside of `object`.
#'
#' @return
#' For `model_fit` and fitted `workflow` input, a modified version of the data
#' set supplied in `data` that contains a `.wts` column with class
#' `importance_weights`. For `tune_results` input, a modified version of the
#' resampling object underlying the tuning results containing a new `.wts` column
#' with propensity values corresponding to each element of the analysis set.
#'
#' @references Barrett M & D'Agostino McGowan L (forthcoming).
#' _Causal Inference in R_. \url{https://www.r-causal.org/}
#' @name weight_propensity.model_fit
NULL

#' @rdname weight_propensity.model_fit
#' @export
weight_propensity <- function(object, wt_fn, ...) {
  UseMethod("weight_propensity")
}

#' @rdname weight_propensity.model_fit
#' @method weight_propensity default
#' @export
weight_propensity.default <- function(object, wt_fn, ...) {
  abort("No known `weight_propensity()` method for this type of object.")
}

#' @noRd
#' @method weight_propensity model_spec
#' @export
weight_propensity.model_spec <- function(object, wt_fn, ...) {
  abort(c(
    "`weight_propensity()` is not well-defined for a model specification.",
    "i" = "Supply `object` to `fit()` before generating propensity weights."
  ))
}

#' @rdname weight_propensity.model_fit
#' @method weight_propensity model_fit
#' @export
weight_propensity.model_fit <- function(object,
                                        wt_fn,
                                        .treated = object$lvl[2],
                                        ...,
                                        data) {
  if (rlang::is_missing(wt_fn) || !is.function(wt_fn)) {
    abort("`wt_fn` must be a function.")
  }

  if (rlang::is_missing(data) || !is.data.frame(data)) {
    abort("`data` must be the data supplied as the data argument to `fit()`.")
  }

  # TODO: I'm not sure we have a way to identify `y` via a model
  #       spec fitted with `fit_xy()`---this will error in that case.
  outcome_name <- object$preproc$y_var

  preds <- predict(object, data, type = "prob")
  preds <- preds[[paste0(".pred_", .treated)]]

  data$.wts <-
    hardhat::importance_weights(
      wt_fn(preds, data[[outcome_name]], .treated, ...)
    )

  data
}
