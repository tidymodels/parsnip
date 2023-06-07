# standalone-survival.R is loaded already. We'll capture the environment where
# the functions of interest reside. We then document them and connect the
# functions to their docs via the use of @alias. After the roxygen comments,
# assign the function by referencing its name in the environment.

surv_ns <- environment(.extract_surv_status)

#' Extract survival time
#'
#' Extract the time component(s) from a [survival::Surv()] object.
#' @name .extract_surv_time
#' @aliases .extract_surv_time
#' @param surv A single [survival::Surv()] object.
#' @return A vector when the type is `"right"` or `"left"` and a tibble otherwise.
#' @export
assign(".extract_surv_time", surv_ns[[".extract_surv_time"]])

#' Extract survival status
#'
#' Extract the status from a [survival::Surv()] object.
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
#' @param eval_time A single numeric value for the evaluation time.
#' @return A two level factor.
#' @details
#' The following three cases can occur:
#'  - **Events**: Evaluation time is greater than or equal to the event time
#' ("it has already happened").
#'  - **Non-events**: Evaluation time is less than the observed time, censored
#'  or not ("nothing has happened yet").
#'  - **Ambiguous outcomes**: Evaluation time is greater than or equal to the
#'  observed censored time ("we don't know if anything might have happened by now").
#'  A missing value is returned for these observations.
#'
#' @references  Graf, E., Schmoor, C., Sauerbrei, W., and Schumacher, M. (1999).
#' "Assessment and Comparison of Prognostic Classification Schemes for Survival Data."
#' _Statistics in Medicine_, 18, 2529-2545.
#' @export
assign(".time_as_binary_event", surv_ns[[".time_as_binary_event"]])
