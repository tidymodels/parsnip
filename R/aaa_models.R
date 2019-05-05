
# initialize model environment

parsnip <- rlang::new_environment()
parsnip$models <- NULL
parsnip$modes <- c("regression", "classification", "unknown")

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
      "`func` should be a named list with names 'pkg' and 'fun' and these",
      "should both be single character strings"
    )

  if (is_missing(func) || !is.list(func) || length(func) != 2)
    stop(msg, call. = FALSE)

  nms <- sort(names(func))
  if (!isTRUE(all.equal(nms, c("fun", "pkg")))) {
    stop(msg, call. = FALSE)
  }
  if (!all(purrr::map_lgl(func, is.character))) {
    stop(msg, call. = FALSE)
  }

  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @export
register_new_model <- function(mod) {
  check_mod_val(mod, new = TRUE)

  current <- get_model_env()

  current$models <- c(current$models, mod)
  current[[mod]] <- dplyr::tibble(engine = character(0), mode = character(0))
  current[[paste0(mod, "_pkg")]] <- character(0)
  current[[paste0(mod, "_modes")]] <- "unknown"
  current[[paste0(mod, "_args")]] <-
    dplyr::tibble(
      engine = character(0),
      parsnip = character(0),
      original = character(0),
      func = list()
    )
  current[[paste0(mod, "_fit")]] <- list()
  current[[paste0(mod, "_predict")]] <- list()

  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @export
register_model_mode <- function(mod, mode) {
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
register_model_engine <- function(mod, mode, eng) {
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
register_model_arg <- function(mod, eng, val, original, func) {
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
register_dependency <- function(mod, pkg) {

}

# ------------------------------------------------------------------------------

#' @export
register_fit <- function(mod, mode, eng, info) {

}


# ------------------------------------------------------------------------------

#' @export
register_pred <- function(mod, mode, eng, type, info) {

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

  invisible(NULL)
}

# ------------------------------------------------------------------------------
