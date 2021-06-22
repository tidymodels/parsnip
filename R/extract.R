#' Extract elements of a parsnip model object
#'
#' @description
#' These functions extract various elements from a parsnip object. If they do
#' not exist yet, an error is thrown.
#'
#' - `extract_parsnip_spec()` returns the parsnip model specification.
#'
#' - `extract_fit_engine()` returns the engine specific fit embedded within
#'   a parsnip model fit. For example, when using [parsnip::linear_reg()]
#'   with the `"lm"` engine, this would return the underlying `lm` object.
#'
#' @param x A parsnip `fit.model_spec()` object.
#' @param ... Not currently used.
#' @return
#' The extracted value from the parsnip object, `x`, as described in the description
#' section.
#'
#' @name extract-parsnip
#' @examples
#' lm_spec <- linear_reg() %>% set_engine("lm")
#' lm_fit <- fit(lm_spec, mpg ~ ., data = mtcars)
#'
#' lm_spec
#' extract_parsnip_spec(lm_fit)
#'
#' extract_fit_engine(lm_fit)
#' lm(mpg ~ ., data = mtcars)
NULL

#' @export
#' @rdname extract-parsnip
extract_parsnip_spec.model_fit <- function(x, ...) {
  if (any(names(x) == "spec")) {
    return(x$spec)
  }
  rlang::abort("The model fit does not have a model spec.")
}


#' @export
#' @rdname extract-parsnip
extract_fit_engine.model_fit <- function(x, ...) {
  if (any(names(x) == "fit")) {
    return(x$fit)
  }
  rlang::abort("The model fit does not have an engine fit.")
}
