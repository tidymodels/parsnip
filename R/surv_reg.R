#' Parametric survival regression
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated in favor of `survival_reg()` which uses the
#' `"censored regression"` mode.
#'
#' `surv_reg()` defines a parametric survival model.
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams nearest_neighbor
#' @param mode A single character string for the prediction outcome mode.
#'  The only possible value for this model is "regression".
#' @param dist A character string for the probability distribution of the
#'  outcome. The default is "weibull".
#'
#' @templateVar modeltype surv_reg
#' @template spec-details
#'
#' @template spec-survival
#'
#' @template spec-references
#'
#' @keywords internal
#' @export
surv_reg <- function(mode = "regression", engine = "survival", dist = NULL) {

  lifecycle::deprecate_warn("0.1.6", "surv_reg()", "survival_reg()")

    args <- list(
      dist = enquo(dist)
    )

    new_model_spec(
      "surv_reg",
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

#' @method update surv_reg
#' @rdname parsnip_update
#' @export
update.surv_reg <- function(object, parameters = NULL, dist = NULL, fresh = FALSE, ...) {

  args <- list(
    dist = enquo(dist)
  )

  update_spec(
    object = object,
    parameters = parameters,
    args_enquo_list = args,
    fresh = fresh,
    cls = "surv_reg",
    ...
  )
}


# ------------------------------------------------------------------------------

#' @export
translate.surv_reg <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'survival'` for translation.")
    engine <- "survival"
  }
  x <- translate.default(x, engine, ...)
  x
}

# ------------------------------------------------------------------------------

#' @export
check_args.surv_reg <- function(object, call = rlang::caller_env()) {

  if (object$engine == "flexsurv") {

    args <- lapply(object$args, rlang::eval_tidy)

    # `dist` has no default in the function
    if (all(names(args) != "dist") || is.null(args$dist))
      object$args$dist <- "weibull"
  }

  invisible(object)
}

# ------------------------------------------------------------------------------

survreg_quant <- function(results, object) {
  pctl <- object$spec$method$pred$quantile$args$p
  n <- nrow(results)
  p <- ncol(results)
  colnames(results) <- names0(p)
  results <-
    results |>
    as_tibble() |>
    mutate(.row = seq_len(n)) |>
    gather(.label, .pred, -.row) |>
    arrange(.row, .label) |>
    mutate(.quantile = rep(pctl, n)) |>
    dplyr::select(-.label)
  .row <- results[[".row"]]
  results <-
    results |>
    dplyr::select(-.row)
  results <- split(results, .row)
  names(results) <- NULL
  tibble(.pred = results)
}

# ------------------------------------------------------------------------------

flexsurv_mean <- function(results, object) {
  results <- unclass(results)
  results <- bind_rows(results)
  results$est
}

flexsurv_quant <- function(results, object) {
  results <- map(results, as_tibble)
  names(results) <- NULL
  results <- map(results, setNames, c(".quantile", ".pred", ".pred_lower", ".pred_upper"))
}

