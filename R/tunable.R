# Unit tests are in extratests
# nocov start

#' @export
tunable.model_spec <- function(x, ...) {
  mod_env <- get_model_env()

  if (is.null(x$engine)) {
    cli::cli_abort("Please declare an engine first using {.fn set_engine}.")
  }

  arg_name <- paste0(mod_type(x), "_args")
  if (!(any(arg_name == names(mod_env)))) {
    cli::cli_abort(
      "The parsnip model database doesn't know about the arguments for
       model {.val {mod_type(x)}}. Was it registered?"
    )
  }

  arg_vals <- mod_env[[arg_name]]
  arg_vals <- arg_vals[arg_vals$engine == x$engine, c("parsnip", "func")]
  names(arg_vals)[names(arg_vals) == "parsnip"] <- "name"
  names(arg_vals)[names(arg_vals) == "func"] <- "call_info"

  extra_args <- c(names(x$args), names(x$eng_args))
  extra_args <- extra_args[!extra_args %in% arg_vals$name]

  extra_args_tbl <-
    tibble::new_tibble(
      list(
        name = extra_args,
        call_info = vector("list", vctrs::vec_size(extra_args))
      ),
      nrow = vctrs::vec_size(extra_args)
    )

  res <- vctrs::vec_rbind(arg_vals, extra_args_tbl)

  res$source <- "model_spec"
  res$component <- mod_type(x)
  res$component_id <- "main"
  res$component_id[!res$name %in% names(x$args)] <- "engine"

  if (nrow(res) > 0) {
    has_info <- purrr::map_lgl(res$call_info, is.null)
    rm_list <- !(has_info & (res$component_id == "main"))

    res <- res[rm_list, ]
  }

  res[, c("name", "call_info", "source", "component", "component_id")]
}

mod_type <- function(.mod) class(.mod)[class(.mod) != "model_spec"][1]

# ------------------------------------------------------------------------------

#' Tunable parameter helpers for extension packages
#'
#' These functions help extension packages customize the tunable parameters
#' for engines they define. They are used within `tunable()` methods to
#' modify the base parameters returned by `tunable.model_spec()`.
#'
#' @param pset,base_result A tibble of tunable parameters, typically from
#'   `NextMethod()` in a model's `tunable()` method.
#' @param engines A tibble of engine-specific parameters to add, with columns
#'   `name`, `call_info`, `source`, `component`, and `component_id`.
#' @param engine A character string naming the engine.
#' @param spec A named list of engine specifications. Each element should be
#'   named after an engine and can contain:
#'   \describe{
#'     \item{add_params}{A tibble of engine parameters to add (passed to
#'       `add_engine_parameters()`).}
#'     \item{updates}{A named list where each element updates a parameter's
#'       `call_info`. Names should match parameter names, and values should
#'       be lists with `pkg`, `fun`, and optionally `range` or `values`.
#'       Use this to change the dials function or range for existing parameters.
#'       Note: for engines defined in parsnip, parameter ranges should be set
#'       when the argument is registered via [set_model_arg()]. The `updates`
#'       mechanism is primarily for extension packages that need to modify
#'       parameters for engines not defined in parsnip.}
#'     \item{replace_fn}{A function with signature `function(base, component)`
#'       that returns a completely new tunable tibble. When present, `add_params`
#'       and `updates` are ignored.}
#'   }
#'
#' @includeRmd man/rmd/tunable_changes.Rmd details
#'
#' @return A tibble of tunable parameters.
#' @keywords internal
#' @name tunable_spec_helpers
#' @export
#' @rdname tunable_spec_helpers
add_engine_parameters <- function(pset, engines) {
  # Remove any base params that have the same name as engine params
  is_engine_param <- pset$name %in% engines$name
  if (any(is_engine_param)) {
    pset <- pset[!is_engine_param, ]
  }
  # Add all engine params
  dplyr::bind_rows(pset, engines)
}

#' @export
#' @rdname tunable_spec_helpers
apply_tunable_spec <- function(base_result, engine, spec) {
  if (is.null(engine) || !engine %in% names(spec)) {
    return(base_result)
  }

  engine_spec <- spec[[engine]]
  res <- base_result

  # Pattern 3: Complete replacement
  if (!is.null(engine_spec$replace_fn)) {
    component <- unique(base_result$component)[1]
    return(engine_spec$replace_fn(base_result, component))
  }

  # Pattern 1: Add engine parameters
  if (!is.null(engine_spec$add_params)) {
    res <- add_engine_parameters(res, engine_spec$add_params)
  }

  # Pattern 2: Update specific parameters
  if (!is.null(engine_spec$updates)) {
    for (param_name in names(engine_spec$updates)) {
      idx <- which(res$name == param_name)
      if (length(idx) > 0) {
        res$call_info[idx] <- list(engine_spec$updates[[param_name]])
      }
    }
  }

  res
}

# ------------------------------------------------------------------------------
# Shared data for brulee engines (used by linear_reg, logistic_reg,
# multinom_reg, mlp)

tune_activations <- c("relu", "tanh", "elu", "log_sigmoid", "tanhshrink")
tune_sched <- c("none", "decay_time", "decay_expo", "cyclic", "step")

brulee_mlp_args <-
  tibble::tibble(
    name = c(
      'epochs',
      'hidden_units',
      'hidden_units_2',
      'activation',
      'activation_2',
      'penalty',
      'mixture',
      'dropout',
      'learn_rate',
      'momentum',
      'batch_size',
      'class_weights',
      'stop_iter',
      'rate_schedule'
    ),
    call_info = list(
      list(pkg = "dials", fun = "epochs", range = c(5L, 500L)),
      list(pkg = "dials", fun = "hidden_units", range = c(2L, 50L)),
      list(pkg = "dials", fun = "hidden_units_2", range = c(2L, 50L)),
      list(pkg = "dials", fun = "activation", values = tune_activations),
      list(pkg = "dials", fun = "activation_2", values = tune_activations),
      list(pkg = "dials", fun = "penalty"),
      list(pkg = "dials", fun = "mixture"),
      list(pkg = "dials", fun = "dropout"),
      list(pkg = "dials", fun = "learn_rate", range = c(-3, -1 / 5)),
      list(pkg = "dials", fun = "momentum", range = c(0.00, 0.99)),
      list(pkg = "dials", fun = "batch_size", range = c(3L, 8L)),
      list(pkg = "dials", fun = "class_weights"),
      list(pkg = "dials", fun = "stop_iter"),
      list(pkg = "dials", fun = "rate_schedule", values = tune_sched)
    )
  ) |>
  dplyr::mutate(source = "model_spec")

brulee_mlp_only_args <-
  tibble::tibble(
    name = c(
      'hidden_units',
      'hidden_units_2',
      'activation',
      'activation_2',
      'dropout'
    )
  )

# nocov end
