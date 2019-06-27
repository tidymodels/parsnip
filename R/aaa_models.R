# Initialize model environments

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
  c("raw", "numeric", "class", "prob", "conf_int", "pred_int", "quantile")

# ------------------------------------------------------------------------------

#' Working with the parsnip model environment
#'
#' These functions read and write to the environment where the package stores
#'  information about model specifications.
#'
#' @param items A character string of objects in the model environment.
#' @param ... Named values that will be assigned to the model environment.
#' @param name A single character value for a new symbol in the model environment.
#' @param value A single value for a new value in the model environment.
#' @keywords internal
#' @references "Making a parsnip model from scratch"
#'  \url{https://tidymodels.github.io/parsnip/articles/articles/Scratch.html}
#' @examples
#' # Access the model data:
#' current_code <- get_model_env()
#' ls(envir = current_code)
#'
#' @keywords internal
#' @export
get_model_env <- function() {
  current <- utils::getFromNamespace("parsnip", ns = "parsnip")
  current
}

#' @rdname get_model_env
#' @keywords internal
#' @export
get_from_env <- function(items) {
  mod_env <- get_model_env()
  rlang::env_get(mod_env, items)
}

#' @rdname get_model_env
#' @keywords internal
#' @export
set_in_env <- function(...) {
  mod_env <- get_model_env()
  rlang::env_bind(mod_env, ...)
}

#' @rdname get_model_env
#' @keywords internal
#' @export
set_env_val <- function(name, value) {
  if (length(name) != 1 || !is.character(name)) {
    stop("`name` should be a single character value.", call. = FALSE)
  }
  mod_env <- get_model_env()
  x <- list(value)
  names(x) <- name
  rlang::env_bind(mod_env, !!!x)
}

# ------------------------------------------------------------------------------

check_eng_val <- function(eng) {
  if (rlang::is_missing(eng) || length(eng) != 1 || !is.character(eng))
    stop("Please supply a character string for an engine name (e.g. `'lm'`)",
         call. = FALSE)
  invisible(NULL)
}


check_model_exists <- function(model) {
  if (rlang::is_missing(model) || length(model) != 1 || !is.character(model)) {
    stop("Please supply a character string for a model name (e.g. `'linear_reg'`)",
         call. = FALSE)
  }

  current <- get_model_env()

  if (!any(current$models == model)) {
    stop("Model `", model, "` has not been registered.", call. = FALSE)
  }

  invisible(NULL)
}

check_model_doesnt_exist <- function(model) {
  if (rlang::is_missing(model) || length(model) != 1 || !is.character(model)) {
    stop("Please supply a character string for a model name (e.g. `'linear_reg'`)",
         call. = FALSE)
  }

  current <- get_model_env()

  if (any(current$models == model)) {
    stop("Model `", model, "` already exists", call. = FALSE)
  }

  invisible(NULL)
}

check_mode_val <- function(mode) {
  if (rlang::is_missing(mode) || length(mode) != 1 || !is.character(mode))
    stop("Please supply a character string for a mode (e.g. `'regression'`)",
         call. = FALSE)
  invisible(NULL)
}

check_engine_val <- function(eng) {
  if (rlang::is_missing(eng) || length(eng) != 1 || !is.character(eng))
    stop("Please supply a character string for an engine (e.g. `'lm'`)",
         call. = FALSE)
  invisible(NULL)
}

check_arg_val <- function(arg) {
  if (rlang::is_missing(arg) || length(arg) != 1 || !is.character(arg))
    stop("Please supply a character string for the argument",
         call. = FALSE)
  invisible(NULL)
}

check_submodels_val <- function(has_submodel) {
  if (!is.logical(has_submodel) || length(has_submodel) != 1) {
    stop("The `submodels` argument should be a single logical.", call. = FALSE)
  }
  invisible(NULL)
}

check_func_val <- function(func) {
  msg <-
    paste(
      "`func` should be a named vector with element 'fun' and the optional ",
      "element 'pkg'. These should both be single character strings."
    )

  if (rlang::is_missing(func) || !is.vector(func) || length(func) > 2)
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

check_fit_info <- function(fit_obj) {
  if (is.null(fit_obj)) {
    stop("The `fit` module cannot be NULL.", call. = FALSE)
  }
  exp_nms <- c("defaults", "func", "interface", "protect")
  if (!isTRUE(all.equal(sort(names(fit_obj)), exp_nms))) {
    stop("The `fit` module should have elements: ",
         paste0("`", exp_nms, "`", collapse = ", "),
         call. = FALSE)
  }

  check_interface_val(fit_obj$interface)
  check_func_val(fit_obj$func)

  if (!is.list(fit_obj$defaults)) {
    stop("The `defaults` element should be a list: ", call. = FALSE)
  }

  invisible(NULL)
}

check_pred_info <- function(pred_obj, type) {
  if (all(type != pred_types)) {
    stop("The prediction type should be one of: ",
         paste0("'", pred_types, "'", collapse = ", "),
         call. = FALSE)
  }

  exp_nms <- c("args", "func", "post", "pre")
  if (!isTRUE(all.equal(sort(names(pred_obj)), exp_nms))) {
    stop("The `predict` module should have elements: ",
         paste0("`", exp_nms, "`", collapse = ", "),
         call. = FALSE)
  }

  if (!is.null(pred_obj$pre) & !is.function(pred_obj$pre)) {
    stop("The `pre` module should be null or a function: ",
         call. = FALSE)
  }
  if (!is.null(pred_obj$post) & !is.function(pred_obj$post)) {
    stop("The `post` module should be null or a function: ",
         call. = FALSE)
  }

  check_func_val(pred_obj$func)

  if (!is.list(pred_obj$args)) {
    stop("The `args` element should be a list. ", call. = FALSE)
  }

  invisible(NULL)
}

check_pkg_val <- function(pkg) {
  if (rlang::is_missing(pkg) || length(pkg) != 1 || !is.character(pkg))
    stop("Please supply a single character vale for the package name",
         call. = FALSE)
  invisible(NULL)
}

check_interface_val <- function(x) {
  exp_interf <- c("data.frame", "formula", "matrix")
  if (length(x) != 1 || !(x %in% exp_interf)) {
    stop("The `interface` element should have a single value of : ",
         paste0("`", exp_interf, "`", collapse = ", "),
         call. = FALSE)
  }
  invisible(NULL)
}


# ------------------------------------------------------------------------------

#' Tools to Register Models
#'
#' These functions are similar to constructors and can be used to validate
#'  that there are no conflicts with the underlying model structures used by the
#'  package.
#'
#' @param model A single character string for the model type (e.g.
#'  `"rand_forest"`, etc).
#' @param mode A single character string for the model mode (e.g. "regression").
#' @param eng A single character string for the model engine.
#' @param arg A single character string for the model argument name.
#' @param has_submodel A single logical for whether the argument
#'  can make predictions on mutiple submodels at once.
#' @param func A named character vector that describes how to call
#'  a function. `func` should have elements `pkg` and `fun`. The
#'  former is optional but is recommended and the latter is
#'  required. For example, `c(pkg = "stats", fun = "lm")` would be
#'  used to invoke the usual linear regression function. In some
#'  cases, it is helpful to use `c(fun = "predict")` when using a
#'  package's `predict` method.
#' @param fit_obj A list with elements `interface`, `protect`,
#'  `func` and `defaults`. See the package vignette "Making a
#'  `parsnip` model from scratch".
#' @param pred_obj A list with elements `pre`, `post`, `func`, and `args`.
#'  See the package vignette "Making a `parsnip` model from scratch".
#' @param type A single character value for the type of prediction. Possible
#'  values are: `class`, `conf_int`, `numeric`, `pred_int`, `prob`, `quantile`,
#'   and `raw`.
#' @param pkg An options character string for a package name.
#' @param parsnip A single character string for the "harmonized" argument name
#'  that `parsnip` exposes.
#' @param original A single character string for the argument name that
#'  underlying model function uses.
#' @param value A list that conforms to the `fit_obj` or `pred_obj` description
#'  above, depending on context.
#' @param pre,post Optional functions for pre- and post-processing of prediction
#'  results.
#' @param ... Optional arguments that should be passed into the `args` slot for
#'  prediction objects.
#' @keywords internal
#' @details These functions are available for users to add their
#'  own models or engines (in package or otherwise) so that they can
#'  be accessed using `parsnip`. This are more thoroughly documented
#'  on the package web site (see references below).
#'
#' In short, `parsnip` stores an environment object that contains
#'  all of the information and code about how models are used (e.g.
#'  fitting, predicting, etc). These functions can be used to add
#'  models to that environment as well as helper functions that can
#'  be used to makes sure that the model data is in the right
#'  format.
#'
#' `check_model_exists()` checks the model value and ensures that the model has
#'  already been registered. `check_model_doesnt_exist()` checks the model value
#'  and also checks to see if it is novel in the environment.
#'
#' @references "Making a parsnip model from scratch"
#'  \url{https://tidymodels.github.io/parsnip/articles/articles/Scratch.html}
#' @examples
#' # set_new_model("shallow_learning_model")
#'
#' # Show the information about a model:
#' show_model_info("rand_forest")
#' @keywords internal
#' @export
set_new_model <- function(model) {
  check_model_doesnt_exist(model)

  current <- get_model_env()

  set_env_val("models", c(current$models, model))
  set_env_val(model, dplyr::tibble(engine = character(0), mode = character(0)))
  set_env_val(
    paste0(model, "_pkgs"),
    dplyr::tibble(engine = character(0), pkg = list())
  )
  set_env_val(paste0(model, "_modes"), "unknown")
  set_env_val(
    paste0(model, "_args"),
    dplyr::tibble(
      engine = character(0),
      parsnip = character(0),
      original = character(0),
      func = list(),
      has_submodel = logical(0)
    )
  )
  set_env_val(
    paste0(model, "_fit"),
    dplyr::tibble(
      engine = character(0),
      mode = character(0),
      value = list()
    )
  )
  set_env_val(
    paste0(model, "_predict"),
    dplyr::tibble(
      engine = character(0),
      mode = character(0),
      type = character(0),
      value = list()
    )
  )

  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @rdname set_new_model
#' @keywords internal
#' @export
set_model_mode <- function(model, mode) {
  check_model_exists(model)
  check_mode_val(mode)

  current <- get_model_env()

  if (!any(current$modes == mode)) {
    current$modes <- unique(c(current$modes, mode))
  }

  set_env_val(
    paste0(model, "_modes"),
    unique(c(get_from_env(paste0(model, "_modes")), mode))
  )
  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @rdname set_new_model
#' @keywords internal
#' @export
set_model_engine <- function(model, mode, eng) {
  check_model_exists(model)
  check_mode_val(mode)
  check_eng_val(eng)
  check_mode_val(eng)

  current <- get_model_env()

  new_eng <- dplyr::tibble(engine = eng, mode = mode)
  old_eng <- get_from_env(model)

  engs <-
    old_eng %>%
    dplyr::bind_rows(new_eng) %>%
    dplyr::distinct()

  set_env_val(model, engs)
  set_model_mode(model, mode)
  invisible(NULL)
}


# ------------------------------------------------------------------------------
#' @importFrom vctrs vec_unique
#' @rdname set_new_model
#' @keywords internal
#' @export
set_model_arg <- function(model, eng, parsnip, original, func, has_submodel) {
  check_model_exists(model)
  check_eng_val(eng)
  check_arg_val(parsnip)
  check_arg_val(original)
  check_func_val(func)
  check_submodels_val(has_submodel)

  current <- get_model_env()
  old_args <- get_from_env(paste0(model, "_args"))

  new_arg <-
    dplyr::tibble(
      engine = eng,
      parsnip = parsnip,
      original = original,
      func = list(func),
      has_submodel = has_submodel
    )

  updated <- try(dplyr::bind_rows(old_args, new_arg), silent = TRUE)
  if (inherits(updated, "try-error")) {
    stop("An error occured when adding the new argument.", call. = FALSE)
  }

  updated <- vctrs::vec_unique(updated)
  set_env_val(paste0(model, "_args"), updated)

  invisible(NULL)
}


# ------------------------------------------------------------------------------

#' @rdname set_new_model
#' @keywords internal
#' @export
set_dependency <- function(model, eng, pkg) {
  check_model_exists(model)
  check_eng_val(eng)
  check_pkg_val(pkg)

  current <- get_model_env()
  model_info <- get_from_env(model)
  pkg_info <- get_from_env(paste0(model, "_pkgs"))

  has_engine <-
    model_info %>%
    dplyr::distinct(engine) %>%
    dplyr::filter(engine == eng) %>%
    nrow()
  if (has_engine != 1) {
    stop("The engine '", eng, "' has not been registered for model '",
         model, "'. ", call. = FALSE)
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

  set_env_val(paste0(model, "_pkgs"), pkg_info)

  invisible(NULL)
}

#' @rdname set_new_model
#' @keywords internal
#' @export
get_dependency <- function(model) {
  check_model_exists(model)
  pkg_name <- paste0(model, "_pkgs")
  if (!any(pkg_name != rlang::env_names(get_model_env()))) {
    stop("`", model, "` does not have a dependency list in parsnip.", call. = FALSE)
  }
  rlang::env_get(get_model_env(), pkg_name)
}


# ------------------------------------------------------------------------------

#' @rdname set_new_model
#' @keywords internal
#' @export
set_fit <- function(model, mode, eng, value) {
  check_model_exists(model)
  check_eng_val(eng)
  check_mode_val(mode)
  check_engine_val(eng)
  check_fit_info(value)

  current <- get_model_env()
  model_info <- get_from_env(model)
  old_fits <- get_from_env(paste0(model, "_fit"))

  has_engine <-
    model_info %>%
    dplyr::filter(engine == eng & mode == !!mode) %>%
    nrow()
  if (has_engine != 1) {
    stop("The combination of engine '", eng, "' and mode '",
         mode, "' has not been registered for model '",
         model, "'. ", call. = FALSE)
  }

  has_fit <-
    old_fits %>%
    dplyr::filter(engine == eng & mode == !!mode) %>%
    nrow()

  if (has_fit > 0) {
    stop("The combination of engine '", eng, "' and mode '",
         mode, "' already has a fit component for model '",
         model, "'. ", call. = FALSE)
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

  set_env_val(
    paste0(model, "_fit"),
    updated
  )

  invisible(NULL)
}

#' @rdname set_new_model
#' @keywords internal
#' @export
get_fit <- function(model) {
  check_model_exists(model)
  fit_name <- paste0(model, "_fit")
  if (!any(fit_name != rlang::env_names(get_model_env()))) {
    stop("`", model, "` does not have a `fit` method in parsnip.", call. = FALSE)
  }
  rlang::env_get(get_model_env(), fit_name)
}

# ------------------------------------------------------------------------------

#' @rdname set_new_model
#' @keywords internal
#' @export
set_pred <- function(model, mode, eng, type, value) {
  check_model_exists(model)
  check_eng_val(eng)
  check_mode_val(mode)
  check_engine_val(eng)
  check_pred_info(value, type)

  current <- get_model_env()
  model_info <- get_from_env(model)
  old_fits <- get_from_env(paste0(model, "_predict"))

  has_engine <-
    model_info %>%
    dplyr::filter(engine == eng & mode == !!mode) %>%
    nrow()
  if (has_engine != 1) {
    stop("The combination of engine '", eng, "' and mode '",
         mode, "' has not been registered for model '",
         model, "'. ", call. = FALSE)
  }

  has_pred <-
    old_fits %>%
    dplyr::filter(engine == eng & mode == !!mode & type == !!type) %>%
    nrow()
  if (has_pred > 0) {
    stop("The combination of engine '", eng, "', mode '",
         mode, "', and type '", type,
         "' already has a prediction component for model '",
         model, "'. ", call. = FALSE)
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

  set_env_val(paste0(model, "_predict"), updated)

  invisible(NULL)
}

#' @rdname set_new_model
#' @keywords internal
#' @export
get_pred_type <- function(model, type) {
  check_model_exists(model)
  pred_name <- paste0(model, "_predict")
  if (!any(pred_name != rlang::env_names(get_model_env()))) {
    stop("`", model, "` does not have any `pred` methods in parsnip.", call. = FALSE)
  }
  all_preds <- rlang::env_get(get_model_env(), pred_name)
  if (!any(all_preds$type == type)) {
    stop("`", model, "` does not have any `", type,
         "` prediction methods in parsnip.", call. = FALSE)
  }
  dplyr::filter(all_preds, type == !!type)
}

# ------------------------------------------------------------------------------

#' @rdname set_new_model
#' @keywords internal
#' @export
show_model_info <- function(model) {
  check_model_exists(model)
  current <- get_model_env()

  cat("Information for `", model, "`\n", sep = "")

  cat(
    " modes:",
    paste0(get_from_env(paste0(model, "_modes")), collapse = ", "),
    "\n\n"
  )

  engines <- get_from_env(model)
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
    cat("\n")
  } else {
    cat(" no registered engines.\n\n")
  }

  args <- get_from_env(paste0(model, "_args"))
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
    cat("\n")
  } else {
    cat(" no registered arguments.\n\n")
  }

  fits <- get_from_env(paste0(model, "_fit"))
  if (nrow(fits) > 0) {
    cat(" fit modules:\n")
    fits %>%
      dplyr::select(-value) %>%
      mutate(engine = paste0("  ", engine)) %>%
      as.data.frame() %>%
      print(row.names = FALSE)
    cat("\n")
  } else {
    cat(" no registered fit modules.\n\n")
  }

  preds <- get_from_env(paste0(model, "_predict"))
  if (nrow(preds) > 0) {
    cat(" prediction modules:\n")
    preds %>%
      dplyr::group_by(mode, engine) %>%
      dplyr::summarize(methods = paste0(sort(type), collapse = ", ")) %>%
      dplyr::ungroup() %>%
      mutate(mode = paste0("  ", mode)) %>%
      as.data.frame() %>%
      print(row.names = FALSE)
    cat("\n")
  } else {
    cat(" no registered prediction modules.\n\n")
  }
  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @rdname set_new_model
#' @keywords internal
#' @export
pred_value_template <-  function(pre = NULL, post = NULL, func, ...) {
  if (rlang::is_missing(func)) {
    stop("Please supply a value to `func`. See `?set_pred`.", call. = FALSE)
  }
  list(pre = pre, post = post, func = func, args = list(...))
}

