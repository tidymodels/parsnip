#' Repair a model call object
#'
#' When the user passes a formula to `fit()` _and_ the underlying model function
#' uses a formula, the call object produced by `fit()` may not be usable by
#' other functions. For example, some arguments may still be quosures and the
#' `data` portion of the call will not correspond to the original data.
#'
#' `repair_call()` call can adjust the model objects call to be usable by other
#' functions and methods.
#' @param x A fitted parsnip model. An error will occur if the underlying model
#'  does not have a `call` element.
#' @param data A data object that is relevant to the call. In most cases, this
#'  is the data frame that was given to parsnip for the model fit (i.e., the
#'  training set data). The name of this data object is inserted into the call.
#' @return A modified `parsnip` fitted model.
#' @examplesIf !parsnip:::is_cran_check()
#'
#' fitted_model <-
#'   linear_reg() |>
#'   set_engine("lm", model = TRUE) |>
#'   fit(mpg ~ ., data = mtcars)
#'
#' # In this call, note that `data` is not `mtcars` and the `model = ~TRUE`
#' # indicates that the `model` argument is an rlang quosure.
#' fitted_model$fit$call
#'
#' # All better:
#' repair_call(fitted_model, mtcars)$fit$call
#' @export
repair_call <- function(x, data) {
  cl <- match.call()
  if (!any(names(x$fit) == "call")) {
    cli::cli_abort("No {.field call} object to modify.")
  }
  if (rlang::is_missing(data)) {
    cli::cli_abort("Please supply a data object to {.arg data}.")
  }
  fit_call <- x$fit$call
  needs_eval <- purrr::map_lgl(fit_call, rlang::is_quosure)
  if (any(needs_eval)) {
    eval_args <- names(needs_eval)[needs_eval]
    for(arg in eval_args) {
      fit_call[[arg]] <- rlang::eval_tidy(fit_call[[arg]])
    }
  }
  if (any(names(fit_call) == "data")) {
    fit_call$data <- cl$data
  }

  x$fit$call <- fit_call
  x
}
