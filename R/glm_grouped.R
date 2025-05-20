#' Fit a grouped binomial outcome from a data set with case weights
#'
#' @description
#' [stats::glm()] assumes that a tabular data set with case weights corresponds
#' to "different observations have different dispersions" (see `?glm`).
#'
#' In some cases, the case weights reflect that the same covariate pattern was
#' observed multiple times (i.e., _frequency weights_). In this case,
#' [stats::glm()] expects the data to be formatted as the number of events for
#' each factor level so that the outcome can be given to the formula as
#' `cbind(events_1, events_2)`.
#'
#' [glm_grouped()] converts data with integer case weights to the expected
#' "number of events" format for binomial data.
#' @param formula A formula object with one outcome that is a two-level factors.
#' @param data A data frame with the outcomes and predictors (but not case
#' weights).
#' @param weights An integer vector of weights whose length is the same as the
#' number of rows in `data`. If it is a non-integer numeric, it will be converted
#' to integer (with a warning).
#' @param ... Options to pass to [stats::glm()]. If `family` is not set, it will
#' automatically be assigned the basic binomial family.
#' @return A object produced by [stats::glm()].
#' @examplesIf !parsnip:::is_cran_check()
#' #----------------------------------------------------------------------------
#' # The same data set formatted three ways
#'
#' # First with basic case weights that, from ?glm, are used inappropriately.
#' ucb_weighted <- as.data.frame(UCBAdmissions)
#' ucb_weighted$Freq <- as.integer(ucb_weighted$Freq)
#' head(ucb_weighted)
#' nrow(ucb_weighted)
#'
#' # Format when yes/no data are in individual rows (probably still inappropriate)
#' library(tidyr)
#' ucb_long <- uncount(ucb_weighted, Freq)
#' head(ucb_long)
#' nrow(ucb_long)
#'
#' # Format where the outcome is formatted as number of events
#' ucb_events <-
#'   ucb_weighted |>
#'   tidyr::pivot_wider(
#'     id_cols = c(Gender, Dept),
#'     names_from = Admit,
#'     values_from = Freq,
#'     values_fill = 0L
#'   )
#' head(ucb_events)
#' nrow(ucb_events)
#'
#' #----------------------------------------------------------------------------
#' # Different model fits
#'
#' # Treat data as separate Bernoulli data:
#' glm(Admit ~ Gender + Dept, data = ucb_long, family = binomial)
#'
#' # Weights produce the same statistics
#' glm(
#'   Admit ~ Gender + Dept,
#'   data = ucb_weighted,
#'   family = binomial,
#'   weights = ucb_weighted$Freq
#' )
#'
#' # Data as binomial "x events out of n trials" format. Note that, to get the same
#' # coefficients, the order of the levels must be reversed.
#' glm(
#'   cbind(Rejected, Admitted) ~ Gender + Dept,
#'   data = ucb_events,
#'   family = binomial
#' )
#'
#' # The new function that starts with frequency weights and gets the correct place:
#' glm_grouped(Admit ~ Gender + Dept, data = ucb_weighted, weights = ucb_weighted$Freq)
#' @export
glm_grouped <- function(formula, data, weights, ...) {
  opts <- list(...)
  # We'll set binomial
  if (!any(names(opts) == "family")) {
    opts$family <- "binomial"
  }

  if (is.null(weights) || !is.numeric(weights)) {
    cli::cli_abort("{.arg weights} should be an integer vector.")
  }
  if (!is.integer(weights)) {
    weights <- as.integer(weights)
    cli::cli_warn("Converting case weights from numeric to integer.")
  }

  terms <- terms(formula)
  all_pred <- all.vars(formula)
  response <- rownames(attr(terms, "factors"))[attr(terms, "response")]
  all_pred <- all_pred[!all_pred %in% response]
  lvls <- levels(data[[response]])

  if (length(lvls) != 2) {
    cli::cli_abort(
      "The response column {.val response} should be a two-level factor."
    )
  }

  all_cols <- c(response, all_pred)
  data <- data[, all_cols, drop = FALSE]
  data$..weights <- weights
  # Reconstruct the new data format (made below) to the grouped formula format
  rlang::f_lhs(formula) <- rlang::call2("cbind", !!!rlang::syms(rev(lvls)))

  data <-
    data |>
    tidyr::pivot_wider(
      id_cols = c(dplyr::all_of(all_pred)),
      names_from = c(dplyr::all_of(response)),
      values_from = "..weights",
      values_fill = 0L
    )
  cl <- rlang::call2("glm", rlang::expr(formula), data = rlang::expr(data), !!!opts)
  rlang::eval_tidy(cl)
}
