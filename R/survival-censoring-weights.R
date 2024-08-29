# ------------------------------------------------------------------------------
# Functions for using inverse probability of censoring weights (IPCW) in
# censored regression models

# ------------------------------------------------------------------------------
# Simple helpers for computing the probability of censoring

# For avoiding extremely large, outlier weights
trunc_probs <- function(probs, trunc = 0.01) {
  is_complt_prob <- !is.na(probs)
  complt_prob <- probs[is_complt_prob]
  non_zero_min <- min(complt_prob[complt_prob > 0])
  if (non_zero_min < trunc) {
    trunc <- non_zero_min / 2
  }
  probs[is_complt_prob] <-
    ifelse(probs[is_complt_prob] <= trunc, trunc, probs[is_complt_prob])
  probs
}

# nocov start
# these are tested in extratests

.check_pred_col <- function(x, call = rlang::env_parent()) {
  if (!any(names(x) == ".pred")) {
    cli::cli_abort(
      "The input should have a list column called {.val .pred}.",
      call = call
    )
  }
  if (!is.list(x$.pred)) {
    cli::cli_abort(
      "The input should have a list column called {.val .pred}.",
      call = call
    )
  }
  req_cols <- c(".eval_time", ".pred_survival")
  if (!all(req_cols %in% names(x$.pred[[1]]))) {
    cli::cli_abort(
      "The {.field .pred} tibbles should have columns {.val req_cols}.",
      call = call
    )
  }
  invisible(NULL)
}

.check_censor_model <- function(x, call = rlang::caller_env()) {
  if (x$spec$mode != "censored regression") {
    cli::cli_abort(
      "The model needs to be for mode {.val censored regression}, not for mode '{x$spec$mode}'.",
      call = call
    )
  }
  nms <- names(x)
  if (!any(nms == "censor_probs")) {
    cli::cli_abort(
      "Please refit the model with {.pkg parsnip} version 1.0.4 or greater.",
      call = call
    )
  }
  invisible(NULL)
}

# ------------------------------------------------------------------------------
# Brier score helpers. Most of this is based off of Graf, E., Schmoor, C.,
# Sauerbrei, W. and Schumacher, M. (1999), Assessment and comparison of
# prognostic classification schemes for survival data. _Statist. Med._, 18:
# 2529-2545.

# We need to use the time of analysis to determine what time to use to evaluate
# the IPCWs.

graf_weight_time_vec <- function(surv_obj, eval_time, eps = 10^-10) {
  event_time <- .extract_surv_time(surv_obj)
  status <- .extract_surv_status(surv_obj)
  is_event_before_t <- event_time <= eval_time & status == 1
  is_censored <- event_time > eval_time

  # Three possible contributions to the statistic from Graf 1999

  # Censoring time before eval_time, no contribution (Graf category 3)
  weight_time <- rep(NA_real_, length(event_time))

  # A real event prior to eval_time (Graf category 1)
  weight_time <- ifelse(is_event_before_t, event_time - eps, weight_time)

  # Observed time greater than eval_time (Graf category 2)
  weight_time <- ifelse(is_censored, eval_time - eps, weight_time)

  weight_time <- ifelse(weight_time < 0, 0, weight_time)

  weight_time
}

# ------------------------------------------------------------------------------
#' Calculations for inverse probability of censoring weights (IPCW)
#'
#' The method of Graf _et al_ (1999) is used to compute weights at specific
#' evaluation times that can be used to help measure a model's time-dependent
#' performance (e.g. the time-dependent Brier score or the area under the ROC
#' curve). This is an internal function.
#'
#' @param predictions A data frame with a column containing a [survival::Surv()]
#' object as well as a list column called `.pred` that contains the data
#' structure produced by [predict.model_fit()].
#' @param cens_predictors Not currently used. A potential future slot for models with
#' informative censoring based on columns in `predictions`.
#' @param object A fitted parsnip model object or fitted workflow with a mode
#' of "censored regression".
#' @param trunc A potential lower bound for the probability of censoring to avoid
#' very large weight values.
#' @param eps A small value that is subtracted from the evaluation time when
#' computing the censoring probabilities. See Details below.
#' @return The same data are returned with the `pred` tibbles containing
#' several new columns:
#'
#' - `.weight_time`: the time at which the inverse censoring probability weights
#'    are computed. This is a function of the observed time and the time of
#'    analysis (i.e., `eval_time`). See Details for more information.
#' - `.pred_censored`: the probability of being censored at `.weight_time`.
#' - `.weight_censored`: The inverse of the censoring probability.
#'
#' @details
#'
#' A probability that the data are censored immediately prior to a specific
#' time is computed. To do this, we must determine what time to
#' make the prediction. There are two time values for each row of the data set:
#' the observed time (either censored or not) and the time that the model is
#' being evaluated at (e.g. the survival function prediction at some time point),
#' which is constant across rows. .
#'
#' From  Graf _et al_ (1999) there are three cases:
#'
#'  - If the observed time is a censoring time and that is before the
#'    evaluation time, the data point should make no contribution to the
#'    performance metric (their "category 3"). These values have a missing
#'    value for their probability estimate (and also for their weight column).
#'
#'  - If the observed time corresponds to an actual event, and that time is
#'    prior to the evaluation time (category 1), the probability of being
#'    censored is predicted at the observed time (minus an epsilon).
#'
#'  - If the observed time is _after_ the evaluation time (category 2), regardless of
#'    the status, the probability of being censored is predicted at the evaluation
#'     time (minus an epsilon).
#'
#' The epsilon is used since, we would not have actual information at time `t`
#' for a data point being predicted at time `t` (only data prior to time `t`
#' should be available).
#'
#' After the censoring probability is computed, the `trunc` option is used to
#' avoid using numbers pathologically close to zero. After this, the weight is
#' computed by inverting the censoring probability.
#'
#' The `eps` argument is used to avoid information leakage when computing the
#' censoring probability. Subtracting a small number avoids using data that
#' would not be known at the time of prediction. For example, if we are making
#' survival probability predictions at `eval_time = 3.0`, we would _not_ know the
#' about the probability of being censored at that exact time (since it has not
#' occurred yet).
#'
#' When creating weights by inverting probabilities, there is the risk that a few
#' cases will have severe outliers due to probabilities close to zero. To
#' mitigate this, the `trunc` argument can be used to put a cap on the weights.
#' If the smallest probability is greater than `trunc`, the probabilities with
#' values less than `trunc` are given that value. Otherwise,  `trunc` is
#' adjusted to be half of the smallest probability and that value is used as the
#' lower bound..
#'
#' Note that if there are `n` rows in `data` and `t` time points, the resulting
#' data, once unnested, has `n * t` rows. Computations will not easily scale
#' well as `t` becomes very large.
#' @references Graf, E., Schmoor, C., Sauerbrei, W. and Schumacher, M. (1999),
#' Assessment and comparison of prognostic classification schemes for survival
#' data. _Statist. Med._, 18: 2529-2545.
#' @export
#' @name censoring_weights
#' @keywords internal
.censoring_weights_graf <- function(object, ...) {
  UseMethod(".censoring_weights_graf")
}

#' @export
#' @rdname censoring_weights
.censoring_weights_graf.default <- function(object, ...) {
  cli::cli_abort(
    "There is no {.fun .censoring_weights_graf} method for objects with class{?es}
     {.cls {class(object)}}."
  )
}

#' @export
#' @rdname censoring_weights
.censoring_weights_graf.model_fit <- function(object,
                                              predictions,
                                              cens_predictors = NULL,
                                              trunc = 0.05, eps = 10^-10, ...) {
  rlang::check_dots_empty()
  .check_censor_model(object)
  truth <- .find_surv_col(predictions)
  .check_censored_right(predictions[[truth]])
  .check_pred_col(predictions)

  if (!is.null(cens_predictors)) {
    cli::cli_warn("{.arg cens_predictors} is not currently used.")
  }
  predictions$.pred <-
    add_graf_weights_vec(object,
                         predictions$.pred,
                         predictions[[truth]],
                         trunc = trunc,
                         eps = eps)
  predictions
}

# ------------------------------------------------------------------------------
# Helpers

add_graf_weights_vec <- function(object, .pred, surv_obj, trunc = 0.05, eps = 10^-10) {
  # Expand the list column to one data frame
  n <- length(.pred)
  num_times <- vctrs::list_sizes(.pred)
  y <- vctrs::list_unchop(.pred)
  y$surv_obj <- vctrs::vec_rep_each(surv_obj, times = num_times)

  names(y)[names(y) == ".time"] <- ".eval_time"   # Temporary

  # Compute the actual time of evaluation
  y$.weight_time <- graf_weight_time_vec(y$surv_obj, y$.eval_time, eps = eps)
  # Compute the corresponding probability of being censored
  y$.pred_censored <- predict(object$censor_probs, time = y$.weight_time, as_vector = TRUE)
  y$.pred_censored <- trunc_probs(y$.pred_censored, trunc = trunc)
  # Invert the probabilities to create weights
  y$.weight_censored = 1 / y$.pred_censored

  # Convert back the list column format
  y$surv_obj <- NULL
  vctrs::vec_chop(y, sizes = num_times)
}

.find_surv_col <- function(x, call = rlang::env_parent(), fail = TRUE) {
  is_lst_col <- purrr::map_lgl(x, purrr::is_list)
  is_surv <- purrr::map_lgl(x[!is_lst_col], .is_surv, fail = FALSE)
  num_surv <- sum(is_surv)
  if (fail && num_surv != 1) {
    cli::cli_abort("There should be a single column of class {.cls Surv}.", call = call)
  }
  names(is_surv)[is_surv]
}

# nocov end
