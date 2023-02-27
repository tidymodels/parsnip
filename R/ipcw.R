# ------------------------------------------------------------------------------
# Functions for using inverse probability of censoring weights (IPCW) in
# censored regression models

# ------------------------------------------------------------------------------
# Low-level survival helpers

#' Helper functions for Surv objects
#'
#' Functions to make it easier to work with [survival::Surv()] objects.
#' @param surv A [survival::Surv()] object
#' @details
#' `.is_censored_right()` always returns a logical while
#' `.check_censored_right()` will fail if `FALSE`.
#'
#' `.extract_status()` will return the data as 0/1 even if the original object
#' used the legacy encoding of 1/2. See [survival::Surv()].
#' @keywords internal
#' @name surv-helpers
#' @return
#' - `.extract_surv_status()` returns a vector.
#' - `.extract_surv_time()` returns a vector when the type is `"right"` or `"left"`
#'    and a tibble otherwise.
#' - Functions starting with `.is_` or `.check_` return logicals although the
#'   latter will fail when `FALSE`.
#' @export

# nocov start
# These are tested in the extratests repo since it would require a dependency
# on the survival package.
.is_censored_right <- function(surv) {
  .check_cens_type(surv, fail = FALSE)
}

#' @rdname surv-helpers
#' @export
.check_censored_right <- function(surv) {
  .check_cens_type(surv, fail = TRUE)
} # will add more as we need them

#' @rdname surv-helpers
#' @export
.extract_surv_time <- function(surv) {
  .is_surv(surv)
  keepers <- c("time", "start", "stop", "time1", "time2")
  res <- surv[, colnames(surv) %in% keepers]
  if (NCOL(res) > 1) {
    res <- tibble::tibble(as.data.frame(res))
  }
  res
}

#' @rdname surv-helpers
#' @export
.extract_surv_status <- function(surv) {
  .is_surv(surv)
  res <-   surv[, "status"]
  un_vals <- sort(unique(res))
  if (identical(un_vals, 1:2) | identical(un_vals, c(1.0, 2.0))) {
    res <- res - 1
  }
  res
}

.is_surv <- function(surv, fail = TRUE) {
  is_surv <- inherits(surv, "Surv")
  if (fail && !is_surv) {
    rlang::abort("The object does not have class `Surv`.", call = NULL)
  }
  is_surv
}

.extract_surv_type <- function(surv) {
  attr(surv, "type")
}

.check_cens_type <- function(surv, type = "right", fail = FALSE) {
  .is_surv(surv)
  obj_type <- .extract_surv_type(surv)
  good_type <- all(obj_type %in% type)
  if (fail && !good_type) {
    c_list <- paste0(type, collapse = ", ")
    msg <- glue::glue("For this usage, the allowed censoring types are: {c_list}")
    rlang::abort(msg, call = NULL)
  }
  good_type
}

# nocov end

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
