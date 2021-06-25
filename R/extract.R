#' Extract elements of a parsnip model object
#'
#' @description
#' These functions extract various elements from a parsnip object. If they do
#' not exist yet, an error is thrown.
#'
#' - `extract_spec_parsnip()` returns the parsnip model specification.
#'
#' - `extract_fit_engine()` returns the engine specific fit embedded within
#'   a parsnip model fit. For example, when using [parsnip::linear_reg()]
#'   with the `"lm"` engine, this would return the underlying `lm` object.
#'
#' @param x A parsnip `fit.model_spec()` object.
#' @param ... Not currently used.
#' @details
#' Extracting the underlying engine fit can be helpful for describing the
#'  model via `print()`, `summarize()`, `plot()`, and so on.
#'
#' However, users should not invoke the `predict()` method on an extracted
#'  model. There may be preprocessing operations that `parsnip` has executed on
#'  the data prior to giving it to the model. Bypassing these can lead to errors
#'  or silently generating incorrect predictions.
#'
#' **Good**:
#' ```r
#'    parsnip_fit %>% predict(new_data)
#' ```
#'
#' **Bad**:
#' ```r
#'    parsnip_fit %>% extract_fit_engine() %>% predict(new_data)
#' ```
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
#' extract_spec_parsnip(lm_fit)
#'
#' extract_fit_engine(lm_fit)
#' lm(mpg ~ ., data = mtcars)
NULL

#' @export
#' @rdname extract-parsnip
extract_spec_parsnip.model_fit <- function(x, ...) {
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
