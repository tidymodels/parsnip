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
