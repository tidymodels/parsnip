#' Resolve a Model Specification for a Computational Engine
#'
#' `translate()` will translate a [model specification][model_spec] into a code
#'  object that is specific to a particular engine (e.g. R package).
#'  It translates generic parameters to their counterparts.
#'
#' @param x A [model specification][model_spec].
#' @param engine The computational engine for the model (see `?set_engine`).
#' @param ... Not currently used.
#' @details
#' `translate()` produces a _template_ call that lacks the specific
#'  argument values (such as `data`, etc). These are filled in once
#'  `fit()` is called with the specifics of the data for the model.
#'  The call may also include `tune()` arguments if these are in
#'  the specification. To handle the `tune()` arguments, you need to use the
#'  [tune package](https://tune.tidymodels.org/). For more information
#'  see <https://www.tidymodels.org/start/tuning/>
#'
#' It does contain the resolved argument names that are specific to
#'  the model fitting function/engine.
#'
#' This function can be useful when you need to understand how
#'  parsnip goes from a generic model specific to a model fitting
#'  function.
#'
#' **Note**: this function is used internally and users should only use it
#'  to understand what the underlying syntax would be. It should not be used
#'  to modify the model specification.
#'
#' @examplesIf !parsnip:::is_cran_check()
#' lm_spec <- linear_reg(penalty = 0.01)
#'
#' # `penalty` is tranlsated to `lambda`
#' translate(lm_spec, engine = "glmnet")
#'
#' # `penalty` not applicable for this model.
#' translate(lm_spec, engine = "lm")
#'
#' # `penalty` is tranlsated to `reg_param`
#' translate(lm_spec, engine = "spark")
#'
#' # with a placeholder for an unknown argument value:
#' translate(linear_reg(penalty = tune(), mixture = tune()), engine = "glmnet")
#'
#' @export

translate <- function(x, ...)
  UseMethod("translate")

#' @rdname translate
#' @export
#' @export translate.default
translate.default <- function(x, engine = x$engine, ...) {
  check_empty_ellipse(...)
  if (is.null(engine)) {
    cli::cli_abort("Please set an engine.")
  }

  mod_name <- specific_model(x)

  x$engine <- engine
  if (x$mode == "unknown") {
    cli::cli_abort("Model code depends on the mode; please specify one.")
  }

  check_spec_mode_engine_val(class(x)[1], x$engine, x$mode)

  if (is.null(x$method)) {
    x$method <- get_model_spec(mod_name, x$mode, engine)
  }

  arg_key <- get_args(mod_name, engine)

  # deharmonize primary arguments
  actual_args <- deharmonize(x$args, arg_key)

  # check secondary arguments to see if they are in the final
  # expression unless there are dots, warn if protected args are
  # being altered
  x$eng_args <- check_eng_args(x$eng_args, x$method$fit, arg_key$original)

  # keep only modified args
  modifed_args <- !purrr::map_lgl(actual_args, null_value)
  actual_args <- actual_args[modifed_args]

  # look for defaults if not modified in other
  if (length(x$method$fit$defaults) > 0) {
    in_other <- names(x$method$fit$defaults) %in% names(x$eng_args)
    x$defaults <- x$method$fit$defaults[!in_other]
  }

  # combine primary, eng_args, and defaults
  protected <- lapply(x$method$fit$protect, function(x) expr(missing_arg()))
  names(protected) <- x$method$fit$protect

  x$method$fit$args <- c(protected, actual_args, x$eng_args, x$defaults)

  x
}

# ------------------------------------------------------------------------------
# new code for revised model data structures

get_model_spec <- function(model, mode, engine) {
  m_env <- get_model_env()

  res <- list()

  libs <- rlang::env_get(m_env, paste0(model, "_pkgs"))
  libs <- vctrs::vec_slice(libs$pkg, libs$engine == engine)
  res$libs <- if (length(libs) > 0) {libs[[1]]} else {NULL}

  fits <- rlang::env_get(m_env, paste0(model, "_fit"))
  fits <- vctrs::vec_slice(fits$value, fits$mode == mode & fits$engine == engine)
  res$fit <- if (length(fits) > 0) {fits[[1]]} else {NULL}

  preds <- rlang::env_get(m_env, paste0(model, "_predict"))
  where <- preds$mode == mode & preds$engine == engine
  types <- vctrs::vec_slice(preds$type, where)
  values <- vctrs::vec_slice(preds$value, where)
  names(values) <- types
  res$pred <- values

  res
}

get_args <- function(model, engine) {
  m_env <- get_model_env()

  args <- rlang::env_get(m_env, paste0(model, "_args"))
  args <- vctrs::vec_slice(args, args$engine == engine)
  args$engine <- NULL

  args
}

# to replace harmonize
deharmonize <- function(args, key) {
  if (length(args) == 0) {
    return(args)
  }

  if (nrow(key) == 0) {
    return(args[integer(0)])
  }

  parsn <- list(parsnip = names(args), order = seq_along(args))
  parsn <- tibble::new_tibble(parsn, nrow = length(args))

  merged <-
    dplyr::left_join(parsn, key, by = "parsnip") |>
    dplyr::arrange(order)

  merged <- merged[!duplicated(merged$order),]

  names(args) <- merged$original
  args[!is.na(merged$original)]
}

add_methods <- function(x, engine) {
  x$engine <- engine
  check_spec_mode_engine_val(class(x)[1], x$engine, x$mode, call = caller_env())
  x$method <- get_model_spec(specific_model(x), x$mode, x$engine)
  x
}


#' Translate names of model tuning parameters
#'
#' This function creates a key that connects the identifiers users make for
#' tuning parameter names, the standardized parsnip parameter names, and the
#' argument names to the underlying fit function for the engine.
#'
#' @param object A workflow or parsnip model specification.
#' @param as_tibble A logical. Should the results be in a tibble (the default)
#' or in a list that can facilitate renaming grid objects?
#' @return A tibble with columns `user`, `parsnip`, and `engine`, or a list
#' with named character vectors `user_to_parsnip` and `parsnip_to_engine`.
#' @keywords internal
#' @examplesIf !parsnip:::is_cran_check() & rlang::is_installed("dials")
#' mod <-
#'  linear_reg(penalty = tune("regularization"), mixture = tune()) |>
#'  set_engine("glmnet")
#'
#' mod |> .model_param_name_key()
#'
#' rn <- mod |> .model_param_name_key(as_tibble = FALSE)
#' rn
#'
#' grid <- tidyr::crossing(regularization = c(0, 1), mixture = (0:3) / 3)
#'
#' grid |>
#'   dplyr::rename(!!!rn$user_to_parsnip)
#'
#' grid |>
#'   dplyr::rename(!!!rn$user_to_parsnip) |>
#'   dplyr::rename(!!!rn$parsnip_to_engine)
#' @export
.model_param_name_key <- function(object, as_tibble = TRUE) {
  if (!inherits(object, c("model_spec", "workflow"))) {
    cli::cli_abort("{.arg object} should be a model specification or workflow.")
  }
  if (inherits(object, "workflow")) {
    object <- hardhat::extract_spec_parsnip(object)
  }

  # To translate from given names/ids in grid to parsnip names:
  params <- object |> hardhat::extract_parameter_set_dials()
  params <- tibble::as_tibble(params) |>
    dplyr::select(user = id, parsnip = name)
  # Go from parsnip names to engine names
  arg_key <- get_from_env(paste0(class(object)[1], "_args")) |>
    dplyr::filter(engine == object$engine) |>
    dplyr::select(engine = original, parsnip)

  res <- dplyr::left_join(params, arg_key, by = "parsnip")
  if (!as_tibble) {
    res0 <- list(user_to_parsnip = res$user, parsnip_to_engine = res$parsnip)
    names(res0$user_to_parsnip) <- res$parsnip
    names(res0$parsnip_to_engine) <- res$engine
    res <- res0
  }
  res
}

