
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
    stop("The `parsnip` model database doesn't know about the arguments for ",
         "model `", mod_type(x), "`. Was it registered?",
         sep = "", call. = FALSE)
  }

  arg_vals <- mod_env[[arg_name]]
  arg_vals <- arg_vals[arg_vals$engine == x$engine, c("parsnip", "func")]
  names(arg_vals)[names(arg_vals) == "parsnip"] <- "name"
  names(arg_vals)[names(arg_vals) == "func"] <- "call_info"

  extra_args <- c(names(x$args), names(x$eng_args))
  extra_args <- extra_args[!extra_args %in% arg_vals$name]

  extra_args_tbl <-
    tibble::new_tibble(
      list(name = extra_args, call_info = vector("list", vctrs::vec_size(extra_args))),
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
  is_engine_param <- pset$name %in% engines$name
  if (any(is_engine_param)) {
    engine_names <- pset$name[is_engine_param]
    pset <- pset[!is_engine_param,]
    pset <-
      dplyr::bind_rows(pset, engines %>% dplyr::filter(name %in% engines$name))
  }
  pset
}

c5_tree_engine_args <-
  tibble::tibble(
    name = c(
      "CF",
      "noGlobalPruning",
      "winnow",
      "fuzzyThreshold",
      "bands"
    ),
    call_info = list(
      list(pkg = "dials", fun = "confidence_factor"),
      list(pkg = "dials", fun = "no_global_pruning"),
      list(pkg = "dials", fun = "predictor_winnowing"),
      list(pkg = "dials", fun = "fuzzy_thresholding"),
      list(pkg = "dials", fun = "rule_bands")
    ),
    source = "model_spec",
    component = "decision_tree",
    component_id = "engine"
  )

c5_boost_engine_args <- c5_tree_engine_args
c5_boost_engine_args$component <- "boost_tree"

xgboost_engine_args <-
  tibble::tibble(
    name = c(
      "alpha",
      "lambda",
      "scale_pos_weight"
    ),
    call_info = list(
      list(pkg = "dials", fun = "penalty_L1"),
      list(pkg = "dials", fun = "penalty_L2"),
      list(pkg = "dials", fun = "scale_pos_weight")
    ),
    source = "model_spec",
    component = "boost_tree",
    component_id = "engine"
  )

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

ranger_engine_args <-
  tibble::tibble(
    name = c(
      "regularization.factor",
      "regularization.usedepth",
      "alpha",
      "minprop",
      "splitrule",
      "num.random.splits"
    ),
    call_info = list(
      list(pkg = "dials", fun = "regularization_factor"),
      list(pkg = "dials", fun = "regularize_depth"),
      list(pkg = "dials", fun = "significance_threshold"),
      list(pkg = "dials", fun = "lower_quantile"),
      list(pkg = "dials", fun = "splitting_rule"),
      list(pkg = "dials", fun = "num_random_splits")
    ),
    source = "model_spec",
    component = "rand_forest",
    component_id = "engine"
  )

randomForest_engine_args <-
  tibble::tibble(
    name = c("maxnodes"),
    call_info = list(
      list(pkg = "dials", fun = "max_nodes")
    ),
    source = "model_spec",
    component = "rand_forest",
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

brulee_mlp_engine_args <-
  tibble::tribble(
    ~name,                                                  ~call_info,
    "momentum",      list(pkg = "dials", fun = "momentum", range = c(0.5, 0.95)),
    "batch_size",      list(pkg = "dials", fun = "batch_size", range = c(3, 10)),
    "stop_iter",                          list(pkg = "dials", fun = "stop_iter"),
    "class_weights",                  list(pkg = "dials", fun = "class_weights"),
    "decay",                             list(pkg = "dials", fun = "rate_decay"),
    "initial",                         list(pkg = "dials", fun = "rate_initial"),
    "largest",                         list(pkg = "dials", fun = "rate_largest"),
    "rate_schedule",                  list(pkg = "dials", fun = "rate_schedule"),
    "step_size",                     list(pkg = "dials", fun = "rate_step_size"),
    "steps",                             list(pkg = "dials", fun = "rate_steps")
  ) %>%
  dplyr::mutate(,
                source = "model_spec",
                component = "mlp",
                component_id = "engine"
  )

brulee_linear_engine_args <-
  brulee_mlp_engine_args %>%
  dplyr::filter(name %in% c("momentum", "batch_size", "stop_iter"))

brulee_logistic_engine_args <-
  brulee_mlp_engine_args %>%
  dplyr::filter(name %in% c("momentum", "batch_size", "stop_iter", "class_weights"))

brulee_multinomial_engine_args <-
  brulee_mlp_engine_args %>%
  dplyr::filter(name %in% c("momentum", "batch_size", "stop_iter", "class_weights"))

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

# ------------------------------------------------------------------------------

#' @export
tunable.linear_reg <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "glmnet") {
    res$call_info[res$name == "mixture"] <-
      list(list(pkg = "dials", fun = "mixture", range = c(0.05, 1.00)))
  } else if (x$engine == "brulee") {
    res <- add_engine_parameters(res, brulee_linear_engine_args)
  }
  res
}

#' @export
tunable.logistic_reg <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "glmnet") {
    res$call_info[res$name == "mixture"] <-
      list(list(pkg = "dials", fun = "mixture", range = c(0.05, 1.00)))
  } else if (x$engine == "brulee") {
    res <- add_engine_parameters(res, brulee_logistic_engine_args)
  }
  res
}

#' @export
tunable.multinomial_reg <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "glmnet") {
    res$call_info[res$name == "mixture"] <-
      list(list(pkg = "dials", fun = "mixture", range = c(0.05, 1.00)))
  } else if (x$engine == "brulee") {
    res <- add_engine_parameters(res, brulee_multinomial_engine_args)
  }
  res
}

#' @export
tunable.boost_tree <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "xgboost") {
    res <- add_engine_parameters(res, xgboost_engine_args)
    res$call_info[res$name == "sample_size"] <-
      list(list(pkg = "dials", fun = "sample_prop"))
    res$call_info[res$name == "learn_rate"] <-
      list(list(pkg = "dials", fun = "learn_rate", range = c(-3, -1/2)))
  } else if (x$engine == "C5.0") {
    res <- add_engine_parameters(res, c5_boost_engine_args)
    res$call_info[res$name == "trees"] <-
      list(list(pkg = "dials", fun = "trees", range = c(1, 100)))
    res$call_info[res$name == "sample_size"] <-
      list(list(pkg = "dials", fun = "sample_prop"))
  } else if (x$engine == "lightgbm") {
    res <- add_engine_parameters(res, lightgbm_engine_args)
    res$call_info[res$name == "sample_size"] <-
      list(list(pkg = "dials", fun = "sample_prop"))
  }
  res
}

#' @export
tunable.rand_forest <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "ranger") {
    res <- add_engine_parameters(res, ranger_engine_args)
  } else if (x$engine == "randomForest") {
    res <- add_engine_parameters(res, randomForest_engine_args)
  } else if (x$engine == "partykit") {
    res <- add_engine_parameters(res, partykit_engine_args)
  } else if (x$engine == "aorsf") {
    res <- add_engine_parameters(res, aorsf_engine_args)
  }
  res
}

#' @export
tunable.mars <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "earth") {
    res <- add_engine_parameters(res, earth_engine_args)
  }
  res
}

#' @export
tunable.decision_tree <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "C5.0") {
    res <- add_engine_parameters(res, c5_tree_engine_args)
  } else if (x$engine == "partykit") {
    res <-
      add_engine_parameters(res,
                            partykit_engine_args %>%
                              dplyr::mutate(component = "decision_tree"))
  }
  res
}

#' @export
tunable.svm_poly <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "kernlab") {
    res$call_info[res$name == "degree"] <-
      list(list(pkg = "dials", fun = "prod_degree", range = c(1L, 3L)))
  }
  res
}


#' @export
tunable.mlp <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "brulee") {
    res <- add_engine_parameters(res, brulee_mlp_engine_args)
    res$call_info[res$name == "learn_rate"] <-
      list(list(pkg = "dials", fun = "learn_rate", range = c(-3, -1/2)))
    res$call_info[res$name == "epochs"] <-
      list(list(pkg = "dials", fun = "epochs", range = c(5L, 500L)))
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

# nocov end

