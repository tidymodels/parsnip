#' @method tune_args model_spec
#' @export
tune_args.model_spec <- function(object, full = FALSE, ...) {

  # use the model_spec top level class as the id
  model_type <- class(object)[1]

  if (length(object$args) == 0L & length(object$eng_args) == 0L) {
    return(tune_tbl())
  }

  # Locate tunable args in spec args and engine specific args
  object$args     <- purrr::map(object$args, convert_args)
  object$eng_args <- purrr::map(object$eng_args, convert_args)

  arg_id <- purrr::map_chr(object$args, find_tune_id)
  eng_arg_id <- purrr::map_chr(object$eng_args, find_tune_id)
  res <- c(arg_id, eng_arg_id)
  res <- ifelse(res == "", names(res), res)

  len_res <- length(res)

  tune_tbl(
    name = names(res),
    tunable = unname(!is.na(res)),
    id = res,
    source = rep("model_spec", len_res),
    component = rep(model_type, len_res),
    component_id = rep(NA_character_, len_res),
    full = full
  )
}



# helpers for tune_args() methods -----------------------------------------
# they also exist in recipes for the `tune_args()` methods there


# If we map over a list or arguments and some are quosures, we get the message
# that "Subsetting quosures with `[[` is deprecated as of rlang 0.4.0"

convert_args <- function(x) {
  if (rlang::is_quosure(x)) {
    x <- rlang::quo_get_expr(x)
  }
  x
}


# useful for standardization and for creating a 0 row tunable tbl
# (i.e. for when there are no steps in a recipe)
tune_tbl <- function(name = character(),
                     tunable = logical(),
                     id = character(),
                     source = character(),
                     component = character(),
                     component_id = character(),
                     full = FALSE,
                     call = caller_env()) {

  check_bool(full, call = call)
  complete_id <- id[!is.na(id)]
  dups <- duplicated(complete_id)
  if (any(dups)) {
    stop("There are duplicate `id` values listed in [tune()]: ",
         paste0("'", unique(complete_id[dups]), "'", collapse = ", "),
         ".", sep = "", call. = FALSE)
  }

  vry_tbl <- tibble::new_tibble(
    list(
      name = as.character(name),
      tunable = as.logical(tunable),
      id = as.character(id),
      source = as.character(source),
      component = as.character(component),
      component_id = as.character(component_id)
    ),
    nrow = length(as.character(name))
  )

  if (!full) {
    vry_tbl <- vry_tbl[vry_tbl$tunable,]
  }

  vry_tbl
}

# Return the `id` arg in tune(); if not specified, then returns "" or if not
# a tunable arg then returns NA_character_
tune_id <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  } else {
    if (rlang::is_quosures(x)) {
      # Try to evaluate to catch things in the global envir.
      .x <- try(purrr::map(x, rlang::eval_tidy), silent = TRUE)
      if (inherits(.x, "try-error")) {
        x <- purrr::map(x, rlang::quo_get_expr)
      } else {
        x <- .x
      }
      if (is.null(x)) {
        return(NA_character_)
      }
    }

    # [tune()] will always return a call object
    if (is.call(x)) {
      if (rlang::is_call_simple(x) && rlang::call_name(x) == "tune") {
        # If an id was specified:
        if (length(x) > 1) {
          return(x[[2]])
        } else {
          # no id
          return("")
        }
        return(x$id)
      } else {
        return(NA_character_)
      }
    }
  }
  NA_character_
}

find_tune_id <- function(x) {

  # STEP 1 - Early exits

  # Early exit for empty elements (like list())
  if (length(x) == 0L) {
    return(NA_character_)
  }

  # turn quosures into expressions before continuing
  if (rlang::is_quosures(x)) {
    # Try to evaluate to catch things in the global envir. If it is a dplyr
    # selector, it will fail to evaluate.
    .x <- try(purrr::map(x, rlang::eval_tidy), silent = TRUE)
    if (inherits(.x, "try-error")) {
      x <- purrr::map(x, rlang::quo_get_expr)
    } else {
      x <- .x
    }
  }

  id <- tune_id(x)
  if (!is.na(id)) {
    return(id)
  }

  if (is.atomic(x) | is.name(x) | length(x) == 1) {
    return(NA_character_)
  }

  # STEP 2 - Recursion

  # tunable_elems <- purrr::map_lgl(x, find_tune)
  tunable_elems <- vector("character", length = length(x))

  # use purrr::map_lgl
  for (i in seq_along(x)) {
    tunable_elems[i] <- find_tune_id(x[[i]])
  }

  tunable_elems <- tunable_elems[!is.na(tunable_elems)]
  if (length(tunable_elems) == 0) {
    tunable_elems <- NA_character_
  }

  if (sum(tunable_elems == "", na.rm = TRUE) > 1) {
    stop(
      "Only one tunable value is currently allowed per argument. ",
      "The current argument has: `",
      paste0(deparse(x), collapse = ""),
      "`.",
      call. = FALSE)
  }

  return(tunable_elems)
}
