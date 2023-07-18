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
