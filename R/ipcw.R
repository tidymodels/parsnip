# ------------------------------------------------------------------------------
# Functions for using inverse probability of censoring weights (IPCW) in
# censored regression models

# ------------------------------------------------------------------------------
# Simple helpers for computing the probability of censoring

# For avoiding extremely large, outlier weights
trunc_probs <- function(probs, trunc = 0.01) {
  complt_prob <- !is.na(probs)
  probs_non_zero <- probs[complt_prob]
  non_zero_min <- min(probs_non_zero[probs_non_zero > 0])
  if (non_zero_min < trunc) {
    trunc <- non_zero_min / 2
  }
  probs[complt_prob] <-
    ifelse(probs[complt_prob] <= trunc, trunc, probs[complt_prob])
  probs
}

.filter_eval_time <- function(eval_time, fail = TRUE) {
  # will still propagate nulls:
  eval_time <- eval_time[!is.na(eval_time)]
  eval_time <- unique(eval_time)
  eval_time <- sort(eval_time)
  eval_time <- eval_time[eval_time >= 0 & is.finite(eval_time)]
  if (fail && identical(eval_time, numeric(0))) {
    rlang::abort(
      "There were no usable evaluation times (finite, non-missing, and >= 0).",
      call = NULL
    )
  }
  eval_time
}

add_dot_row_to_weights <- function(dat, rows = NULL) {
  if (is.null(rows)) {
    dat <- add_rowindex(dat)
  } else {
    m <- length(rows)
    n <- nrow(dat)
    if (m != n) {
      rlang::abort(
        glue::glue(
          "The length of 'rows' ({m}) should be equal to the number of rows in 'data' ({n})"
        )
      )
    }
    dat$.row <- rows
  }
  dat
}

.check_censor_model <- function(x) {
  nms <- names(x)
  if (!any(nms == "censor_probs")) {
    rlang::abort("Please refit the model with parsnip version 1.0.4 or greater.")
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

graf_weight_time <- function(surv_obj, eval_time, rows = NULL, eps = 10^-10) {
  event_time <- .extract_surv_time(surv_obj)
  status <- .extract_surv_status(surv_obj)
  is_event_before_t <- event_time <= eval_time & status == 1
  is_censored <- event_time > eval_time

  # Three possible contributions to the statistic from Graf 1999

  # Censoring time before eval_time, no contribution (Graf category 3)
  weight_time <- rep(NA_real_, length(event_time))

  # A real event prior to predict time (Graf category 1)
  weight_time[is_event_before_t] <- event_time[is_event_before_t] - eps

  # Observed time greater than eval_time (Graf category 2)
  weight_time[is_censored] <- eval_time - eps

  res <- tibble::tibble(surv = surv_obj, weight_time = weight_time, eval_time)
  add_dot_row_to_weights(res, rows)
}

# ------------------------------------------------------------------------------
#' Calculations for inverse probability of censoring weights (IPCW)
#'
#' The method of Graf _et al_ (1999) is used to compute weights at specific
#' evaluation times that can be used to help measure a model's time-dependent
#' performance (e.g. the time-dependent Brier score or the area under the ROC
#' curve).
#' @param data A data frame with a column containing a [survival::Surv()] object.
#' @param predictors Not currently used. A potential future slot for models with
#' informative censoring based on columns in `data`.
#' @param rows An optional integer vector with length equal to the number of
#' rows in `data` that is used to index the original data. The default is to
#' use a fresh index on data (i.e. `1:nrow(data)`).
#' @param eval_time A vector of non-negative times at which we should
#' compute the probability of censoring and the corresponding weights.
#' @param object A fitted parsnip model object or fitted workflow with a mode
#' of "censored regression".
#' @param trunc A potential lower bound for the probability of censoring to avoid
#' very large weight values.
#' @param eps A small value that is subtracted from the evaluation time when
#' computing the censoring probabilities. In doing so, the censoring probability
#' prediction avoids information leakage by avoiding data that would not be
#' known at the time of prediction.
#' @return A tibble with columns `.row`, `eval_time`, `.prob_cens` (the
#' probability of being censored just prior to the evaluation time), and
#' `.weight_cens` (the inverse probability of censoring weight).
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
#'  - If the observed time corresponds to an actual event, and it is _after_
#'    the evaluation time (category 2), the probability of being
#'    censored is predicted at the evaluation time (minus an epsilon).
#'
#' The epsilon is used since, we would not have actual information at time `t`
#' for a data point being predicted at time `t` (only data prior to time `t`
#' should be available).
#'
#' After the censoring probability is computed, the `trunc` option is used to
#' avoid using numbers pathologically close to zero. After this, the weight is
#' computed by inverting the censoring probability.
#'
#' Note that if there are `n` rows in `data` and `t` time points, the resulting
#' data has `n * t` rows. Computations will not easily scale well as `t` becomes
#' large.
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
  cls <- paste0("'", class(object), "'", collapse = ", ")
  msg <- paste("There are no `.censoring_weights_graf` for objects with class(es):",
               cls)
  rlang::abort(msg, call = FALSE)
}


#' @export
#' @rdname censoring_weights
.censoring_weights_graf.workflow <- function(object,
                                            data,
                                            eval_time,
                                            rows = NULL,
                                            predictors = NULL,
                                            trunc = 0.05, eps = 10^-10, ...) {
  if (is.null(object$fit$fit)) {
    rlang::abort("The workflow does not have a model fit object.", call = FALSE)
  }
  .censoring_weights_graf(object$fit$fit, data, eval_time, rows, predictors, trunc, eps)
}

#' @export
#' @rdname censoring_weights
.censoring_weights_graf.model_fit <- function(object,
                                             data,
                                             eval_time,
                                             rows = NULL,
                                             predictors = NULL,
                                             trunc = 0.05, eps = 10^-10, ...) {
  rlang::check_dots_empty()
  .check_censor_model(object)
  if (!is.null(predictors)) {
    rlang::warn("The 'predictors' argument to the survival weighting function is not currently used.", call = FALSE)
  }
  eval_time <- .filter_eval_time(eval_time)  # TODO maybe this should be the check function

  truth <- object$preproc$y_var
  surv_data <- dplyr::select(data, dplyr::all_of(!!truth)) %>% setNames("surv")
  .check_censored_right(surv_data$surv)

  purrr::map_dfr(eval_time,
                 ~ graf_weight_time(surv_data$surv, .x, eps = eps, rows = rows))  %>%
    dplyr::mutate(
      .prob_cens = predict(object$censor_probs, time = weight_time, as_vector = TRUE),
      .prob_cens = trunc_probs(.prob_cens, trunc),
      .weight_cens = 1 / .prob_cens
    )  %>%
    dplyr::select(.row, eval_time, .prob_cens, .weight_cens)
}


