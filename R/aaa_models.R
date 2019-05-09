# Initialize model environment

# ------------------------------------------------------------------------------

## Rules about model-related information

### Definitions:

# - the model is the model type (e.g. "rand_forest", "linear_reg", etc)
# - the model's mode is the species of model such as "classification" or "regression"
# - the engines are within a model and mode and describe the method/implementation
#   of the model in question. These are often R package names.

### The package dependencies are model- and engine-specific. They are used across modes

### The `fit` information is a list of data that is needed to fit the model. This
### information is specific to an engine and mode.

### The `predict` information is also list of data that is needed to make some sort
### of prediction on the model object. The possible types are contained in `pred_types`
### and this information is specific to the engine, mode, and type (although there
### are no types across different modes).

# ------------------------------------------------------------------------------


parsnip <- rlang::new_environment()
parsnip$models <- NULL
parsnip$modes <- c("regression", "classification", "unknown")

# ------------------------------------------------------------------------------

pred_types <-
  c("raw", "numeric", "class", "link", "prob", "conf_int", "pred_int", "quantile")

# ------------------------------------------------------------------------------

#' @export
get_model_env <- function() {
  current <- utils::getFromNamespace("parsnip", ns = "parsnip")
  # current <- get("parsnip")
  current
}

#' @export
check_mod_val <- function(mod, new = FALSE, existance = FALSE) {
  if (is_missing(mod) || length(mod) != 1)
    stop("Please supply a character string for a model name (e.g. `'linear_reg'`)",
         call. = FALSE)

  if (new | existance) {
    current <- get_model_env()
  }

  if (new) {
    if (any(current$models == mod)) {
      stop("Model `", mod, "` already exists", call. = FALSE)
    }
  }

  if (existance) {
    current <- get_model_env()
    if (!any(current$models == mod)) {
      stop("Model `", mod, "` has not been registered.", call. = FALSE)
    }
  }

  invisible(NULL)
}

#' @export
check_mode_val <- function(mode) {
  if (is_missing(mode) || length(mode) != 1)
    stop("Please supply a character string for a mode (e.g. `'regression'`)",
         call. = FALSE)
  invisible(NULL)
}

#' @export
check_engine_val <- function(eng) {
  if (is_missing(eng) || length(eng) != 1)
    stop("Please supply a character string for an engine (e.g. `'lm'`)",
         call. = FALSE)
  invisible(NULL)
}

#' @export
check_arg_val <- function(arg) {
  if (is_missing(arg) || length(arg) != 1)
    stop("Please supply a character string for the argument",
         call. = FALSE)
  invisible(NULL)
}

#' @export
check_func_val <- function(func) {
  msg <-
    paste(
      "`func` should be a named vector with element 'fun' and the optional ",
      "element 'pkg'. These should both be single character strings."
    )

  if (is_missing(func) || !is.vector(func) || length(func) > 2)
    stop(msg, call. = FALSE)

  nms <- sort(names(func))

  if (all(is.null(nms)))  {
    stop(msg, call. = FALSE)
  }

  if (length(func) == 1) {
    if (isTRUE(any(nms != "fun"))) {
      stop(msg, call. = FALSE)
    }
  } else {
    if (!isTRUE(all.equal(nms, c("fun", "pkg")))) {
      stop(msg, call. = FALSE)
    }
  }


  if (!all(purrr::map_lgl(func, is.character))) {
    stop(msg, call. = FALSE)
  }

  invisible(NULL)
}

#' @export
check_fit_info <- function(x) {
  if (is.null(x)) {
    stop("The `fit` module cannot be NULL.", call. = FALSE)
  }
  exp_nms <- c("defaults", "func", "interface", "protect")
  if (!isTRUE(all.equal(sort(names(x)), exp_nms))) {
    stop("The `fit` module should have elements: ",
         paste0("`", exp_nms, "`", collapse = ", "),
         call. = FALSE)
  }

  exp_interf <- c("data.frame", "formula", "matrix")
  if (length(x$interface) > 1) {
    stop("The `interface` element should have a single value of : ",
         paste0("`", exp_interf, "`", collapse = ", "),
         call. = FALSE)
  }
  if (!any(x$interface == exp_interf)) {
    stop("The `interface` element should have a value of : ",
         paste0("`", exp_interf, "`", collapse = ", "),
         call. = FALSE)
  }
  check_func_val(x$func)

  if (!is.list(x$defaults)) {
    stop("The `defaults` element should be a list: ", call. = FALSE)
  }

  invisible(NULL)
}

#' @export
check_pred_info <- function(x, type) {
  if (all(type != pred_types)) {
    stop("The prediction type should be one of: ",
         paste0("'", pred_types, "'", collapse = ", "),
         call. = FALSE)
  }

  exp_nms <- c("args", "func", "post", "pre")
  if (!isTRUE(all.equal(sort(names(x)), exp_nms))) {
    stop("The `predict` module should have elements: ",
         paste0("`", exp_nms, "`", collapse = ", "),
         call. = FALSE)
  }

  if (!is.null(x$pre) & !is.function(x$pre)) {
    stop("The `pre` module should be null or a function: ",
         call. = FALSE)
  }
  if (!is.null(x$post) & !is.function(x$post)) {
    stop("The `post` module should be null or a function: ",
         call. = FALSE)
  }

  check_func_val(x$func)

  if (!is.list(x$args)) {
    stop("The `args` element should be a list. ", call. = FALSE)
  }

  invisible(NULL)
}


#' @export
check_pkg_val <- function(x) {
  if (is_missing(x) || length(x) != 1 || !is.character(x))
    stop("Please supply a single character vale for the package name",
         call. = FALSE)
  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @export
set_new_model <- function(mod) {
  check_mod_val(mod, new = TRUE)

  current <- get_model_env()

  current$models <- c(current$models, mod)
  current[[mod]] <- dplyr::tibble(engine = character(0), mode = character(0))
  current[[paste0(mod, "_pkgs")]] <- dplyr::tibble(engine = character(0), pkg = list())
  current[[paste0(mod, "_modes")]] <- "unknown"
  current[[paste0(mod, "_args")]] <-
    dplyr::tibble(
      engine = character(0),
      parsnip = character(0),
      original = character(0),
      func = list()
    )
  current[[paste0(mod, "_fit")]] <-
    dplyr::tibble(
      engine = character(0),
      mode = character(0),
      value = list()
    )
  current[[paste0(mod, "_predict")]] <-
    dplyr::tibble(
      engine = character(0),
      mode = character(0),
      type = character(0),
      value = list()
    )

  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @export
set_model_mode <- function(mod, mode) {
  check_mod_val(mod, existance = TRUE)
  check_mode_val(mode)

  current <- get_model_env()

  if (!any(current$modes == mode)) {
    current$modes <- unique(c(current$modes, mode))
  }
  current[[paste0(mod, "_modes")]] <-
    unique(c(current[[paste0(mod, "_modes")]], mode))

  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @export
set_model_engine <- function(mod, mode, eng) {
  check_mod_val(mod, existance = TRUE)
  check_mode_val(mode)
  check_mode_val(eng)

  current <- get_model_env()

  new_eng <- dplyr::tibble(engine = eng, mode = mode)
  old_eng <- current[[mod]]
  engs <-
    old_eng %>%
    dplyr::bind_rows(new_eng) %>%
    dplyr::distinct()

  current[[mod]] <- engs

  invisible(NULL)
}


# ------------------------------------------------------------------------------

#' @export
set_model_arg <- function(mod, eng, val, original, func) {
  check_mod_val(mod, existance = TRUE)
  check_arg_val(val)
  check_arg_val(original)
  check_func_val(func)

  current <- get_model_env()
  old_args <- current[[paste0(mod, "_args")]]

  new_arg <-
    dplyr::tibble(
      engine = eng,
      parsnip = val,
      original = original,
      func = list(func)
    )

  updated <- try(dplyr::bind_rows(old_args, new_arg), silent = TRUE)
  if (inherits(updated, "try-error")) {
    stop("An error occured when adding the new argument.", call. = FALSE)
  }

  updated <- dplyr::distinct(updated, engine, parsnip, original)

  current[[paste0(mod, "_args")]] <- updated

  invisible(NULL)
}


# ------------------------------------------------------------------------------

#' @export
set_dependency <- function(mod, eng, pkg) {
  check_mod_val(mod, existance = TRUE)
  check_pkg_val(pkg)

  current <- get_model_env()
  model_info <- current[[mod]]
  pkg_info <- current[[paste0(mod, "_pkgs")]]

  has_engine <-
    model_info %>%
    dplyr::distinct(engine) %>%
    dplyr::filter(engine == eng) %>%
    nrow()
  if (has_engine != 1) {
    stop("The engine '", eng, "' has not been registered for model '",
         mod, "'. ", call. = FALSE)
  }

  existing_pkgs <-
    pkg_info %>%
    dplyr::filter(engine == eng)

  if (nrow(existing_pkgs) == 0) {
    pkg_info <-
      pkg_info %>%
      dplyr::bind_rows(tibble(engine = eng, pkg = list(pkg)))

  } else {
    old_pkgs <- existing_pkgs
    existing_pkgs$pkg[[1]] <- c(pkg, existing_pkgs$pkg[[1]])
    pkg_info <-
      pkg_info %>%
      dplyr::filter(engine != eng) %>%
      dplyr::bind_rows(existing_pkgs)
  }
  current[[paste0(mod, "_pkgs")]] <- pkg_info

  invisible(NULL)
}

#' @export
get_dependency <- function(mod) {
  check_mod_val(mod, existance = TRUE)
  pkg_name <- paste0(mod, "_pkgs")
  if (!any(pkg_name != rlang::env_names(get_model_env()))) {
    stop("`", mod, "` does not have a dependency list in parsnip.", call. = FALSE)
  }
  rlang::env_get(get_model_env(), pkg_name)
}


# ------------------------------------------------------------------------------

#' @export
set_fit <- function(mod, mode, eng, value) {
  check_mod_val(mod, existance = TRUE)
  check_mode_val(mode)
  check_engine_val(eng)
  check_fit_info(value)

  current <- get_model_env()
  model_info <- current[[paste0(mod)]]
  old_fits <- current[[paste0(mod, "_fit")]]

  has_engine <-
    model_info %>%
    dplyr::filter(engine == eng & mode == !!mode) %>%
    nrow()
  if (has_engine != 1) {
    stop("set_fit The combination of engine '", eng, "' and mode '",
         mode, "' has not been registered for model '",
         mod, "'. ", call. = FALSE)
  }

  has_fit <-
    old_fits %>%
    dplyr::filter(engine == eng & mode == !!mode) %>%
    nrow()

  if (has_fit > 0) {
    stop("The combination of engine '", eng, "' and mode '",
         mode, "' already has a fit component for model '",
         mod, "'. ", call. = FALSE)
  }

  new_fit <-
    dplyr::tibble(
      engine = eng,
      mode = mode,
      value = list(value)
    )

  updated <- try(dplyr::bind_rows(old_fits, new_fit), silent = TRUE)
  if (inherits(updated, "try-error")) {
    stop("An error occured when adding the new fit module", call. = FALSE)
  }

  current[[paste0(mod, "_fit")]] <- updated

  invisible(NULL)
}

#' @export
get_fit <- function(mod) {
  check_mod_val(mod, existance = TRUE)
  fit_name <- paste0(mod, "_fit")
  if (!any(fit_name != rlang::env_names(get_model_env()))) {
    stop("`", mod, "` does not have a `fit` method in parsnip.", call. = FALSE)
  }
  rlang::env_get(get_model_env(), fit_name)
}

# ------------------------------------------------------------------------------

#' @export
set_pred <- function(mod, mode, eng, type, value) {
  check_mod_val(mod, existance = TRUE)
  check_mode_val(mode)
  check_engine_val(eng)
  check_pred_info(value, type)

  current <- get_model_env()
  model_info <- current[[paste0(mod)]]
  old_fits <- current[[paste0(mod, "_predict")]]

  has_engine <-
    model_info %>%
    dplyr::filter(engine == eng & mode == !!mode) %>%
    nrow()
  if (has_engine != 1) {
    stop("The combination of engine '", eng, "' and mode '",
         mode, "' has not been registered for model '",
         mod, "'. ", call. = FALSE)
  }

  has_pred <-
    old_fits %>%
    dplyr::filter(engine == eng & mode == !!mode & type == !!type) %>%
    nrow()
  if (has_pred > 0) {
    stop("The combination of engine '", eng, "', mode '",
         mode, "', and type '", type,
         "' already has a prediction component for model '",
         mod, "'. ", call. = FALSE)
  }

  new_fit <-
    dplyr::tibble(
      engine = eng,
      mode = mode,
      type = type,
      value = list(value)
    )

  updated <- try(dplyr::bind_rows(old_fits, new_fit), silent = TRUE)
  if (inherits(updated, "try-error")) {
    stop("An error occured when adding the new fit module", call. = FALSE)
  }

  current[[paste0(mod, "_predict")]] <- updated

  invisible(NULL)
}

#' @export
get_pred_type <- function(mod, type) {
  check_mod_val(mod, existance = TRUE)
  pred_name <- paste0(mod, "_predict")
  if (!any(pred_name != rlang::env_names(get_model_env()))) {
    stop("`", mod, "` does not have any `pred` methods in parsnip.", call. = FALSE)
  }
  all_preds <- rlang::env_get(get_model_env(), pred_name)
  if (!any(all_preds$type == type)) {
    stop("`", mod, "` does not have any `", type,
         "` prediction methods in parsnip.", call. = FALSE)
  }
  dplyr::filter(all_preds, type == !!type)
}

# ------------------------------------------------------------------------------

#' @export
validate_model <- function(mod) {
  # check for consistency across engines, modes, args, etc
}

# ------------------------------------------------------------------------------

#' @export
show_model_info <- function(mod) {
  check_mod_val(mod, existance = TRUE)
  current <- get_model_env()

  cat("Information for `", mod, "`\n", sep = "")

  cat(
    " modes:",
    paste0(current[[paste0(mod, "_modes")]], collapse = ", "),
    "\n"
  )

  engines <- current[[paste0(mod)]]
  if (nrow(engines) > 0) {
    cat(" engines: \n")
    engines %>%
      dplyr::mutate(
        mode = format(paste0(mode, ": "))
      ) %>%
      dplyr::group_by(mode) %>%
      dplyr::summarize(
        engine = paste0(sort(engine), collapse = ", ")
      ) %>%
      dplyr::mutate(
        lab = paste0("   ", mode, engine, "\n")
      ) %>%
      dplyr::ungroup() %>%
      dplyr::pull(lab) %>%
      cat(sep = "")
  } else {
    cat(" no registered engines yet.")
  }

  args <- current[[paste0(mod, "_args")]]
  if (nrow(args) > 0) {
    cat(" arguments: \n")
    args %>%
      dplyr::select(engine, parsnip, original) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        engine = format(paste0("   ", engine, ": ")),
        parsnip = paste0("      ", format(parsnip), " --> ", original, "\n")
      ) %>%
      dplyr::group_by(engine) %>%
      dplyr::mutate(
        engine2 = ifelse(dplyr::row_number() == 1, engine, ""),
        parsnip = ifelse(dplyr::row_number() == 1, paste0("\n", parsnip), parsnip),
        lab = paste0(engine2, parsnip)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::pull(lab) %>%
      cat(sep = "")
  } else {
    cat(" no registered arguments yet.")
  }

  fits <- current[[paste0(mod, "_fits")]]
  if (nrow(fits) > 0) {

  } else {
    cat(" no registered fit modules yet.")
  }

  invisible(NULL)
}

# ------------------------------------------------------------------------------
