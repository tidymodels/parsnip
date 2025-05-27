#' Extract elements of a parsnip model object
#'
#' @description
#' These functions extract various elements from a parsnip object. If they do
#' not exist yet, an error is thrown.
#'
#' - `extract_spec_parsnip()` returns the parsnip [model specification][model_spec].
#'
#' - `extract_fit_engine()` returns the engine specific fit embedded within
#'   a parsnip model fit. For example, when using [parsnip::linear_reg()]
#'   with the `"lm"` engine, this returns the underlying `lm` object.
#'
#' - `extract_parameter_dials()` returns a single dials parameter object.
#'
#' - `extract_parameter_set_dials()` returns a set of dials parameter objects.
#'
#' - `extract_fit_time()` returns a tibble with fit times. The fit times
#'   correspond to the time for the parsnip engine to fit and do not include
#'   other portions of the elapsed time in [parsnip::fit.model_spec()].
#'
#' @param x A parsnip `model_fit` object or a parsnip `model_spec` object.
#' @param parameter A single string for the parameter ID.
#' @param summarize A logical for whether the elapsed fit time should be
#'   returned as a single row or multiple rows. Doesn't support `FALSE` for
#'   parsnip models.
#' @param ... Not currently used.
#' @details
#' Extracting the underlying engine fit can be helpful for describing the
#'  model (via `print()`, `summary()`, `plot()`, etc.) or for variable
#'  importance/explainers.
#'
#' However, users should not invoke the `predict()` method on an extracted
#'  model. There may be preprocessing operations that parsnip has executed on
#'  the data prior to giving it to the model. Bypassing these can lead to errors
#'  or silently generating incorrect predictions.
#'
#' **Good**:
#' ```r
#'    parsnip_fit |> predict(new_data)
#' ```
#'
#' **Bad**:
#' ```r
#'    parsnip_fit |> extract_fit_engine() |> predict(new_data)
#' ```
#' @return
#' The extracted value from the parsnip object, `x`, as described in the description
#' section.
#'
#' @name extract-parsnip
#' @examplesIf !parsnip:::is_cran_check() & rlang::is_installed("dials")
#' lm_spec <- linear_reg() |> set_engine("lm")
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
  cli::cli_abort("The model fit does not have a model spec.", .internal = TRUE)
}


#' @export
#' @rdname extract-parsnip
extract_fit_engine.model_fit <- function(x, ...) {
  if (any(names(x) == "fit")) {
    return(x$fit)
  }
  cli::cli_abort("The model fit does not have  an engine fit.", .internal = TRUE)
}

#' @export
#' @rdname extract-parsnip
extract_parameter_set_dials.model_spec <- function(x, ...) {
  if (!spec_is_loaded(spec = x)) {
    prompt_missing_implementation(
      spec = x,
      prompt = cli::cli_abort,
      call = NULL
    )
  }

  all_args <- generics::tunable(x)
  tuning_param <- generics::tune_args(x)

  res <-
    dplyr::inner_join(
      tuning_param |> dplyr::select(-tunable, -component_id),
      all_args,
      by = c("name", "source", "component")
    ) |>
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

#' @export
#' @rdname extract-parsnip
extract_fit_time.model_fit <- function(x, summarize = TRUE, ...) {
  elapsed <- x[["elapsed"]][["elapsed"]][["elapsed"]]

  if (is.na(elapsed) || is.null(elapsed)) {
    cli::cli_abort(
      "This model was fit before {.fun extract_fit_time} was added."
    )
  }

  dplyr::tibble(
    stage_id = class(x$spec)[1],
    elapsed = elapsed
  )
}
