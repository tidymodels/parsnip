#' Chronos-2 pretrained forecasting model
#'
#' @description
#' `tabular_chronos()` defines a pretrained time-series forecasting model that
#' produces quantile (distributional) forecasts. The network has fixed
#' pretrained weights, so no training is performed; the historical ("context")
#' data is ingested at fit time and the model forecasts a fixed horizon. This
#' function can fit quantile regression and regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("tabular_chronos")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @param mode A single character string for the type of model. The possible
#'   values for this model are `"quantile regression"` (the natural mode, which
#'   returns a `hardhat::quantile_pred()`) and `"regression"` (which returns the
#'   median point forecast). The mode must be set before fitting; for
#'   `"quantile regression"` it is set with
#'   `set_mode("quantile regression", quantile_levels = ...)`.
#' @param engine A single character string specifying what computational engine
#'   to use for fitting. The only valid value is `"brulee"`.
#'
#' @details
#' Unlike the other models in this package, Chronos-2 is pretrained and has no
#' tuning parameters. Forecast configuration is supplied through the engine with
#' [set_engine()], e.g. `set_engine("brulee", prediction_length = 14)`.
#' The available engine arguments mirror [brulee::brulee_chronos()]:
#' `prediction_length`, `id_column`, `timestamp_column`, `model_id`, `revision`,
#' `device`, and `cache_dir`. The `quantile_levels` are taken from the mode (via
#' [set_mode()]) and forwarded to the fit automatically.
#'
#' On first use the engine downloads the pretrained weights (about 500MB) and
#' caches them locally.
#'
#' The \pkg{parsnip} interface forecasts a __single series__. `predict()`
#' returns one row per horizon step (`.pred_quantile` or `.pred`), which cannot
#' unambiguously represent more than one series; supplying data with multiple
#' `id_column` values is therefore an error. For multi-series forecasting, call
#' [brulee::brulee_chronos()] directly, where the id column is retained in the
#' output.
#'
#' @templateVar modeltype tabular_chronos
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("tabular_chronos")} [brulee::brulee_chronos()]
#'
#' @references
#' Ansari, A. F., Shchur, O., Küken, J., Auer, A., Han, B., Mercado, P., et al.
#' (2025). "Chronos-2: From univariate to universal forecasting."
#' _arXiv preprint_ arXiv:2510.15821.
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("tabular_chronos")
#'
#' # Quantile (distributional) forecast
#' tabular_chronos() |>
#'   set_engine("brulee", prediction_length = 14) |>
#'   set_mode("quantile regression", quantile_levels = (1:9) / 10)
#'
#' # Median point forecast
#' tabular_chronos() |>
#'   set_engine("brulee", prediction_length = 14) |>
#'   set_mode("regression")
#' @export
tabular_chronos <-
  function(mode = "unknown", engine = "brulee") {
    args <- list()

    new_model_spec(
      "tabular_chronos",
      args = args,
      eng_args = NULL,
      mode = mode,
      user_specified_mode = !missing(mode),
      method = NULL,
      engine = engine,
      user_specified_engine = !missing(engine)
    )
  }

# ------------------------------------------------------------------------------

#' @method update tabular_chronos
#' @rdname parsnip_update
#' @inheritParams tabular_chronos
#' @export
update.tabular_chronos <-
  function(object, parameters = NULL, fresh = FALSE, ...) {
    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = list(),
      fresh = fresh,
      cls = "tabular_chronos",
      ...
    )
  }

# ------------------------------------------------------------------------------

set_new_model("tabular_chronos")
set_model_mode("tabular_chronos", mode = "quantile regression")
set_model_mode("tabular_chronos", mode = "regression")
