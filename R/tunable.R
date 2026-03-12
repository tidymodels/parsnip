# Unit tests are in extratests
# nocov start

#' @export
tunable.model_spec <- function(x, ...) {
  mod_env <- get_model_env()

  if (is.null(x$engine)) {
    stop("Please declare an engine first using `set_engine()`.", call. = FALSE)
  }

  arg_name <- paste0(mod_type(x), "_args")
  if (!(any(arg_name == names(mod_env)))) {
    stop(
      "The `parsnip` model database doesn't know about the arguments for ",
      "model `",
      mod_type(x),
      "`. Was it registered?",
      sep = "",
      call. = FALSE
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

add_engine_parameters <- function(pset, engines) {
  # Remove any engine params that already exist in pset (to avoid duplicates)
  is_engine_param <- pset$name %in% engines$name
  if (any(is_engine_param)) {
    pset <- pset[!is_engine_param, ]
  }
  # Always add the engine parameters
  dplyr::bind_rows(pset, engines)
}

# Engine-specific tunable parameters for external packages (bonsai, censored).
# These engines are registered in external packages, so we add their tunable
# parameters at runtime.

lightgbm_engine_args <-
  tibble::tibble(
    name = c(
      "num_leaves"
    ),
    call_info = list(
      list(pkg = "dials", fun = "num_leaves")
    ),
    source = "model_spec",
    component = "boost_tree",
    component_id = "engine"
  )

catboost_engine_args <-
  tibble::tibble(
    name = c(
      "max_leaves",
      "l2_leaf_reg"
    ),
    call_info = list(
      list(pkg = "dials", fun = "num_leaves"),
      list(pkg = "dials", fun = "penalty", range = c(-4, 1))
    ),
    source = "model_spec",
    component = "boost_tree",
    component_id = "engine"
  )

partykit_engine_args <-
  tibble::tibble(
    name = c(
      "mincriterion",
      "teststat",
      "testtype"
    ),
    call_info = list(
      list(pkg = "dials", fun = "conditional_min_criterion"),
      list(pkg = "dials", fun = "conditional_test_statistic"),
      list(pkg = "dials", fun = "conditional_test_type")
    ),
    source = "model_spec",
    component = "rand_forest",
    component_id = "engine"
  )

aorsf_engine_args <-
  tibble::tibble(
    name = c(
      "split_min_stat"
    ),
    call_info = list(
      list(pkg = "dials", fun = "conditional_min_criterion")
    ),
    source = "model_spec",
    component = "rand_forest",
    component_id = "engine"
  )

ordinalForest_engine_args <-
  tibble::tibble(
    name = c(
      "naive",
      "nsets",
      "npermtrial",
      "ntreeperdiv",
      "nbest",
      "perffunction"
    ),
    call_info = list(
      list(pkg = "ordered", fun = "naive_scores"),
      list(pkg = "ordered", fun = "num_scores"),
      list(pkg = "ordered", fun = "num_score_perms"),
      list(pkg = "ordered", fun = "num_score_trees"),
      list(pkg = "ordered", fun = "num_scores_best"),
      list(pkg = "ordered", fun = "ord_metric")
    ),
    source = "model_spec",
    component = "rand_forest",
    component_id = "engine"
  )

earth_engine_args <-
  tibble::tibble(
    name = c("nk"),
    call_info = list(
      list(pkg = "dials", fun = "max_num_terms")
    ),
    source = "model_spec",
    component = "mars",
    component_id = "engine"
  )

flexsurvspline_engine_args <-
  tibble::tibble(
    name = c("k"),
    call_info = list(
      list(pkg = "dials", fun = "num_knots")
    ),
    source = "model_spec",
    component = "survival_reg",
    component_id = "engine"
  )

polr_engine_args <-
  tibble::tibble(
    name = c("method"),
    call_info = list(
      list(pkg = "dials", fun = "ordinal_link")
    ),
    source = "model_spec",
    component = "ordinal_reg",
    component_id = "engine"
  )

ordinalNet_engine_args <-
  tibble::tibble(
    name = c(
      "link",
      "family",
      "lambdaVals",
      "alpha"
    ),
    call_info = list(
      list(pkg = "dials", fun = "ordinal_link"),
      list(pkg = "dials", fun = "odds_link"),
      list(pkg = "dials", fun = "penalty"),
      list(pkg = "dials", fun = "mixture")
    ),
    source = "model_spec",
    component = "ordinal_reg",
    component_id = "engine"
  )

# ------------------------------------------------------------------------------
# used for brulee engines:

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

# ------------------------------------------------------------------------------

#' @export
tunable.linear_reg <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "brulee") {
    res <-
      brulee_mlp_args |>
      dplyr::anti_join(brulee_mlp_only_args, by = "name") |>
      dplyr::filter(name != "class_weights") |>
      dplyr::mutate(
        component = "linear_reg",
        component_id = ifelse(
          name %in% names(formals("linear_reg")),
          "main",
          "engine"
        )
      ) |>
      dplyr::select(name, call_info, source, component, component_id)
  }
  res
}

#' @export
tunable.logistic_reg <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "brulee") {
    res <-
      brulee_mlp_args |>
      dplyr::anti_join(brulee_mlp_only_args, by = "name") |>
      dplyr::mutate(
        component = "logistic_reg",
        component_id = ifelse(
          name %in% names(formals("logistic_reg")),
          "main",
          "engine"
        )
      ) |>
      dplyr::select(name, call_info, source, component, component_id)
  }
  res
}

#' @export
tunable.multinom_reg <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "brulee") {
    res <-
      brulee_mlp_args |>
      dplyr::anti_join(brulee_mlp_only_args, by = "name") |>
      dplyr::mutate(
        component = "multinom_reg",
        component_id = ifelse(
          name %in% names(formals("multinom_reg")),
          "main",
          "engine"
        )
      ) |>
      dplyr::select(name, call_info, source, component, component_id)
  }
  res
}

#' @export
tunable.boost_tree <- function(x, ...) {
  res <- NextMethod()
  # lightgbm and catboost are registered in bonsai
  if (x$engine == "lightgbm") {
    res <- add_engine_parameters(res, lightgbm_engine_args)
    res$call_info[res$name == "sample_size"] <-
      list(list(pkg = "dials", fun = "sample_prop", range = c(0.5, 1.0)))
    res$call_info[res$name == "learn_rate"] <-
      list(list(pkg = "dials", fun = "learn_rate", range = c(-3, -1 / 2)))
  } else if (x$engine == "catboost") {
    res <- add_engine_parameters(res, catboost_engine_args)
    res$call_info[res$name == "learn_rate"] <-
      list(list(pkg = "dials", fun = "learn_rate", range = c(-3, -1 / 2)))
    res$call_info[res$name == "sample_size"] <-
      list(list(pkg = "dials", fun = "sample_prop", range = c(0.5, 1.0)))
  }
  res
}

#' @export
tunable.rand_forest <- function(x, ...) {
  res <- NextMethod()
  # partykit and aorsf are registered in bonsai/censored
  if (x$engine == "partykit") {
    res <- add_engine_parameters(res, partykit_engine_args)
  } else if (x$engine == "aorsf") {
    res <- add_engine_parameters(res, aorsf_engine_args)
  } else if (x$engine == "ordinalForest") {
    res <- add_engine_parameters(res, ordinalForest_engine_args)
  }
  res
}

#' @export
tunable.decision_tree <- function(x, ...) {
  res <- NextMethod()
  # partykit is registered in bonsai/censored
  if (x$engine == "partykit") {
    res <-
      add_engine_parameters(
        res,
        partykit_engine_args |>
          dplyr::mutate(component = "decision_tree")
      )
  } else if (x$engine == "rpartScore") {
    res <- add_engine_parameters(res, rpartScore_engine_args)
  }
  res
}

#' @export
tunable.mlp <- function(x, ...) {
  res <- NextMethod()
  if (grepl("brulee", x$engine)) {
    res <-
      brulee_mlp_args |>
      dplyr::mutate(
        component = "mlp",
        component_id = ifelse(name %in% names(formals("mlp")), "main", "engine")
      ) |>
      dplyr::select(name, call_info, source, component, component_id)
    if (x$engine == "brulee") {
      res <- res[!grepl("_2", res$name), ]
    }
  }
  res
}

#' @export
tunable.survival_reg <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "flexsurvspline") {
    res <- add_engine_parameters(res, flexsurvspline_engine_args)
  }
  res
}

#' @export
tunable.ordinal_reg <- function(x, ...) {
  res <- NextMethod()
  # REVIEW: Check that this is necessary.
  if (x$engine == "polr") {
    res <- add_engine_parameters(res, polr_engine_args)
  }
  # REVIEW: Check that this is necessary.
  if (x$engine == "ordinalNet") {
    res <- add_engine_parameters(res, ordinalNet_engine_args)
  }
  res
}

# nocov end
