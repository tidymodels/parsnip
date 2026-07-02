#' Regularization Learning Network
#'
#' @description
#' `tabular_rln()` defines a single-hidden-layer neural network where each
#' weight learns its own adaptive regularization coefficient. This function can
#' fit regression models only.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("tabular_rln")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @param mode A single character string for the type of model. The only
#'   valid value is `"regression"`.
#' @param engine A single character string specifying what computational engine
#'   to use for fitting. The only valid value is `"brulee"`.
#' @param hidden_units An integer for the number of units in the single hidden
#'   layer (>= 1).
#' @param penalty_type A string for the regularization norm: `"L1"` (default
#'   in `brulee`) or `"L2"`. L1 is recommended by the original paper.
#' @param penalty_average A positive numeric value for the target geometric mean
#'   of the per-weight regularization coefficients, on the natural scale.
#'   Best tuned on the log10 scale via [dials::penalty_average()].
#' @param step_rate A positive numeric value for the step size used to update
#'   the per-weight regularization coefficients, on the natural scale.
#'   Best tuned on the log10 scale via [dials::step_rate()].
#' @param activation A character string for the activation function between
#'   the hidden and output layers (e.g., `"relu"`, `"elu"`, `"tanh"`).
#' @param epochs An integer for the number of training iterations.
#' @param learn_rate A positive number for the learning rate.
#' @param rate_schedule A character string for the learning rate schedule
#'   (e.g., `"none"`, `"decay_time"`, `"cyclic"`).
#' @param momentum A number between 0 (inclusive) and 1 for the momentum
#'   parameter used by the optimizer.
#' @param batch_size An integer for the number of training samples used per
#'   gradient update step.
#' @param stop_iter A non-negative integer for the number of iterations without
#'   improvement before stopping training early.
#'
#' @templateVar modeltype tabular_rln
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("tabular_rln")}
#'
#' @references
#' Shavitt, I., & Segal, E. (2018). Regularization learning networks: Deep
#' learning for tabular datasets. _Advances in Neural Information Processing
#' Systems_, 31, 1379-1389.
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("tabular_rln")
#'
#' tabular_rln(hidden_units = 10L, penalty_average = 1e-8)
#' @export
tabular_rln <-
  function(
    mode = "regression",
    engine = "brulee",
    hidden_units = NULL,
    penalty_type = NULL,
    penalty_average = NULL,
    step_rate = NULL,
    activation = NULL,
    epochs = NULL,
    learn_rate = NULL,
    rate_schedule = NULL,
    momentum = NULL,
    batch_size = NULL,
    stop_iter = NULL
  ) {
    args <- list(
      hidden_units = enquo(hidden_units),
      penalty_type = enquo(penalty_type),
      penalty_average = enquo(penalty_average),
      step_rate = enquo(step_rate),
      activation = enquo(activation),
      epochs = enquo(epochs),
      learn_rate = enquo(learn_rate),
      rate_schedule = enquo(rate_schedule),
      momentum = enquo(momentum),
      batch_size = enquo(batch_size),
      stop_iter = enquo(stop_iter)
    )

    new_model_spec(
      "tabular_rln",
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

#' @method update tabular_rln
#' @rdname parsnip_update
#' @inheritParams tabular_rln
#' @export
update.tabular_rln <-
  function(
    object,
    parameters = NULL,
    hidden_units = NULL,
    penalty_type = NULL,
    penalty_average = NULL,
    step_rate = NULL,
    activation = NULL,
    epochs = NULL,
    learn_rate = NULL,
    rate_schedule = NULL,
    momentum = NULL,
    batch_size = NULL,
    stop_iter = NULL,
    fresh = FALSE,
    ...
  ) {
    args <- list(
      hidden_units = enquo(hidden_units),
      penalty_type = enquo(penalty_type),
      penalty_average = enquo(penalty_average),
      step_rate = enquo(step_rate),
      activation = enquo(activation),
      epochs = enquo(epochs),
      learn_rate = enquo(learn_rate),
      rate_schedule = enquo(rate_schedule),
      momentum = enquo(momentum),
      batch_size = enquo(batch_size),
      stop_iter = enquo(stop_iter)
    )

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "tabular_rln",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @method check_args tabular_rln
#' @export
check_args.tabular_rln <- function(object, call = rlang::caller_env()) {
  args <- lapply(object$args, rlang::eval_tidy)

  if (!is.null(args$penalty_type) && !args$penalty_type %in% c("L1", "L2")) {
    cli::cli_abort(
      "{.arg penalty_type} must be {.val L1} or {.val L2}, not {.val {args$penalty_type}}.",
      call = call
    )
  }

  check_number_decimal(
    args$penalty_average,
    min = 0,
    allow_null = TRUE,
    call = call,
    arg = "penalty_average"
  )

  check_number_decimal(
    args$step_rate,
    min = 0,
    allow_null = TRUE,
    call = call,
    arg = "step_rate"
  )

  invisible(object)
}

# ------------------------------------------------------------------------------

set_new_model("tabular_rln")
set_model_mode("tabular_rln", mode = "regression")
