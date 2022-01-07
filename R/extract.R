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
#'   with the `"lm"` engine, this returns the underlying `lm` object.
#'
#' - `extract_parameter_dials()` returns a single dials parameter object.
#'
#' - `extract_parameter_set_dials()` returns a set of dials parameter objects.
#'
#' @param x A parsnip `model_fit` object or a parsnip `model_spec` object.
#' @param parameter A single string for the parameter ID.
#' @param ... Not currently used.
#' @details
#' Extracting the underlying engine fit can be helpful for describing the
#'  model (via `print()`, `summary()`, `plot()`, etc.) or for variable
#'  importance/explainers.
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
  rlang::abort("Internal error: The model fit does not have a model spec.")
}


#' @export
#' @rdname extract-parsnip
extract_fit_engine.model_fit <- function(x, ...) {
  if (any(names(x) == "fit")) {
    return(x$fit)
  }
  rlang::abort("Internal error: The model fit does not have an engine fit.")
}

#' @export
#' @rdname extract-parsnip
extract_parameter_set_dials.model_spec <- function(x, ...) {
  all_args <- generics::tunable(x)
  tuning_param <- generics::tune_args(x)

  res <-
    dplyr::inner_join(
      tuning_param %>% dplyr::select(-tunable, -component_id),
      all_args,
      by = c("name", "source", "component")
    ) %>%
    mutate(object = purrr::map(call_info, eval_call_info))

  dials::parameters_constr(
    res$name,
    res$id,
    res$source,
    res$component,
    res$component_id,
    res$object
  )
}

eval_call_info <-  function(x) {
  if (!is.null(x)) {
    # Look for other options
    allowed_opts <- c("range", "trans", "values")
    if (any(names(x) %in% allowed_opts)) {
      opts <- x[names(x) %in% allowed_opts]
    } else {
      opts <- list()
    }
    res <- try(rlang::eval_tidy(rlang::call2(x$fun, .ns = x$pkg, !!!opts)), silent = TRUE)
    if (inherits(res, "try-error")) {
      stop(paste0("Error when calling ", x$fun, "(): ", as.character(res)))
    }
  } else {
    res <- NA
  }
  res
}

#' @export
#' @rdname extract-parsnip
extract_parameter_dials.model_spec <- function(x, parameter, ...) {
  extract_parameter_dials(extract_parameter_set_dials(x), parameter)
}
