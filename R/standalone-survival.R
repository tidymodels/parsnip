# ---
# repo: tidymodels/parsnip
# file: standalone-survival.R
# last-updated: 2023-12-08
# license: https://unlicense.org
# ---

# This file provides a portable set of helper functions for survival analysis.
#

# ## Changelog
# 2023-12-08
# * move .filter_eval_time to this file
#
# 2023-11-09
# * make sure survival vectors are unnamed.
#
# 2023-06-14
# * removed time to factor conversion
#
# 2023-05-18
# * added time to factor conversion
#
# 2023-02-28:
# * Initial version
#
# ------------------------------------------------------------------------------
#
# @param surv A [survival::Surv()] object
# @details
# `.is_censored_right()` always returns a logical while
# `.check_censored_right()` will fail if `FALSE`.
#
# `.extract_status()` will return the data as 0/1 even if the original object
# used the legacy encoding of 1/2. See [survival::Surv()].

# @return
# - `.extract_surv_status()` returns a vector.
# - `.extract_surv_time()` returns a vector when the type is `"right"` or `"left"`
#    and a tibble otherwise.
# - Functions starting with `.is_` or `.check_` return logicals although the
#   latter will fail when `FALSE`.

# nocov start
# These are tested in the extratests repo since it would require a dependency
# on the survival package. https://github.com/tidymodels/extratests/pull/78
.is_surv <- function(surv, fail = TRUE, call = rlang::caller_env()) {
  is_surv <- inherits(surv, "Surv")
  if (!is_surv && fail) {
    rlang::abort("The object does not have class `Surv`.", call = call)
  }
  is_surv
}

.extract_surv_type <- function(surv) {
  attr(surv, "type")
}

.check_cens_type <-
  function(surv,
           type = "right",
           fail = TRUE,
           call = rlang::caller_env()) {
    .is_surv(surv, call = call)
    obj_type <- .extract_surv_type(surv)
    good_type <- all(obj_type %in% type)
    if (!good_type && fail) {
      c_list <- paste0("'", type, "'")
      msg <-
        cli::format_inline("For this usage, the allowed censoring type{?s}
                            {?is/are}: {c_list}")
      rlang::abort(msg, call = call)
    }
    good_type
  }

.is_censored_right <- function(surv) {
  .check_cens_type(surv, type = "right", fail = FALSE)
}

.check_censored_right <- function(surv, call = rlang::caller_env()) {
  .check_cens_type(surv, type = "right", fail = TRUE, call = call)
} # will add more as we need them

.extract_surv_time <- function(surv) {
  .is_surv(surv)
  keepers <- c("time", "start", "stop", "time1", "time2")
  cols <- colnames(surv)[colnames(surv) %in% keepers]
  res <- surv[, cols, drop = FALSE]
  if (length(cols) > 1) {
    res <- tibble::tibble(as.data.frame(res))
  } else {
    res <- as.numeric(res)
  }
  res
}

.extract_surv_status <- function(surv) {
  .is_surv(surv)
  res <-   surv[, "status"]
  un_vals <- sort(unique(res))
  event_type_to_01 <-
    !(.extract_surv_type(surv) %in% c("interval", "interval2", "mstate"))
  if (
    event_type_to_01 &&
    (identical(un_vals, 1:2) | identical(un_vals, c(1.0, 2.0))) ) {
    res <- res - 1
  }
  unname(res)
}

# ------------------------------------------------------------------------------

# @param eval_time A vector of numeric time points
# @details
# `.filter_eval_time` checks the validity of the time points.
#
# @return A potentially modified vector of time points.
.filter_eval_time <- function(eval_time, fail = TRUE) {
  if (!is.null(eval_time)) {
    eval_time <- as.numeric(eval_time)
  }
  eval_time_0 <- eval_time
  # will still propagate nulls:
  eval_time <- eval_time[!is.na(eval_time)]
  eval_time <- eval_time[eval_time >= 0 & is.finite(eval_time)]
  eval_time <- unique(eval_time)
  if (fail && identical(eval_time, numeric(0))) {
    cli::cli_abort(
      "There were no usable evaluation times (finite, non-missing, and >= 0).",
      call = NULL
    )
  }
  if (!identical(eval_time, eval_time_0)) {
    diffs <- setdiff(eval_time_0, eval_time)
    cli::cli_warn("There {?was/were} {length(diffs)} inappropriate evaluation
                  time point{?s} that {?was/were} removed.", call = NULL)

  }
  eval_time
}
# nocov end
