# We'll load the standalone file then capture the environment where the functions
# reside. We then document them and connect the functions to their docs via the
# use of @alias. After the roxygen comments, assign the function by referencing
# its name in the environment.

#' @include standalone-survival.R
NULL

surv_ns <- environment(.extract_surv_status)

#' Get survival time
#'
#' Pull out the time component(s) from a [survival::Surv()] object.
#' @name .extract_surv_time
#' @aliases .extract_surv_time
#' @param surv A single [survival::Surv()] object.
#' @return A vector when the type is `"right"` or `"left"` and a tibble otherwise.
#' @export
assign(".extract_surv_time", surv_ns[[".extract_surv_time"]])

#' Get survival status
#'
#' Pull out the status from a [survival::Surv()] object.
#' @name .extract_surv_status
#' @aliases .extract_surv_status
#' @param surv A single [survival::Surv()] object.
#' @return A numeric vector.
#' @export
assign(".extract_surv_status", surv_ns[[".extract_surv_status"]])

#' Convert survival objects to binary factors
#'
#' For a given evaluation time, convert a [survival::Surv()] object to a binary
#' factor with levels `"event"` and `"non-event"`.
#'
#' @name .time_as_binary_event
#' @param surv A single [survival::Surv()] object.
#' @param eval_time A single numeric value for a landmark time.
#' @return A two level factor.
#' @details
#' The following three cases can occur:
#'  - **Definitive non-events**: event times (i.e., not censored) are less than
#'  the evaluation time ("it hasn't happened yet")
#'  - **Definitive events**: Observed times (censored or not) are greater than
#'  the evaluation time ("it happens sometime after now").
#'  - **Ambiguous outcomes**: Observed censored time is less than the evaluation
#'  time ("maybe it happens, maybe not"). A missing value is returned for these
#'  observations.
#' @export
assign(".time_as_binary_event", surv_ns[[".time_as_binary_event"]])
