#' Turn a parsnip model object into a tidy tibble
#'
#' This method tidies the model in a parsnip model object, if it exists.
#'
#' @inheritParams generics::tidy
#'
#' @return a tibble
#' @export
tidy.model_fit <- function(x, ...) generics::tidy(x$fit, ...)


#' Construct a single row summary "glance" of a model, fit, or other object
#'
#' This method glances the model in a parsnip model object, if it exists.
#'
#' @inheritParams generics::glance
#'
#' @return a tibble
#' @export
glance.model_fit <- function(x, ...) generics::glance(x$fit, ...)
