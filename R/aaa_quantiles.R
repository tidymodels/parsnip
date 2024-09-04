# Helpers for quantile regression models

check_quantile_level <- function(x, object, call) {
  if ( object$mode != "quantile regression" ) {
    return(invisible(TRUE))
  } else {
    if ( is.null(x) ) {
      cli::cli_abort("In {.fn check_mode}, at least one value of
      {.arg quantile_level} must be specified for quantile regression models.")
    }
  }
  x <- sort(unique(x))
  # TODO we need better vectorization here, otherwise we get things like:
  # "Error during wrapup: i In index: 2." in the traceback.
  res <-
    purrr::map(x,
               ~ check_number_decimal(.x, min = 0, max = 1,
                                      arg = "quantile_level", call = call,
                                      allow_infinite = FALSE)
  )
  x
}


# -------------------------------------------------------------------------
# A column vector of quantiles with an attribute

#' @export
vec_ptype_abbr.vctrs_quantiles <- function(x, ...) "qntls"

#' @export
vec_ptype_full.vctrs_quantiles <- function(x, ...) "quantiles"

#' @importFrom rlang is_list is_double !!!
new_vec_quantiles <- function(values = list(), quantile_levels = double()) {
  quantile_levels <- vctrs::vec_cast(quantile_levels, double())
  num_values <- vctrs::vec_size_common(!!!values)
  if (length(quantile_levels) != num_values) {
    cli::cli_abort(
      "{.arg quantile_levels} must have the same length as {.arg values}. It has
      length {.val {length(quantile_levels)}} not {.val {num_values}}."
    )
  }
  purrr::walk(
    quantile_levels,
    ~ check_number_decimal(.x, min = 0, max = 1, arg = "quantile_levels")
  )
  vctrs::new_vctr(
    values, quantile_levels = quantile_levels, class = "vctrs_quantiles"
  )
}


#' A vector containing sets of quantiles
#'
#' @param values A data.frame/matrix/vector of values. If a named data.frame,
#'   the column names will be used as the `quantile_levels` if those are missing.
#' @param quantile_levels A vector of probabilities corresponding to `values`.
#'   May be `NULL` if `values` is a named data.frame.
#'
#' @export
#'
#' @examples
#' preds <- vec_quantiles(list(1:4, 8:11), c(.2, .4, .6, .8))
#'
#' vec_quantiles(1:4, 1:4 / 5)
vec_quantiles <- function(values, quantile_levels = NULL) {
  check_vec_quantiles_inputs(values, quantile_levels)
# TODO save call reference
  quantile_levels <- vctrs::vec_cast(quantile_levels, double())

  num_lvls <- length(quantile_levels)

  if (is.data.frame(values) || (is.matrix(values) && length(dim(values)) == 2)) {
    values <- lapply(vctrs::vec_chop(values), function(v) sort(drop(v)))
  } else if (is.list(values)) {
    values <- values
  } else if (is.null(dim(values))) {
    if (length(values) != num_lvls) {
      values <- vctrs::vec_chop(values)
    }
  } else {
    cli::cli_abort(
      "{.arg values} must be a {.cls list}, {.cls matrix}, or {.cls data.frame},
      not a {.cls {class(values)}}."
    )
  }
  new_vec_quantiles(values, quantile_levels)
}

check_vec_quantiles_inputs <- function(values, levels) {
  if (is.null(levels)) {
    if (!is.data.frame(values)) {
      cli::cli_abort("If {.arg quantile_levels} is `NULL`, {.arg values} must
                     be a data.frame.")
    }
    levels <- as.numeric(names(values))
    if (any(is.na(levels))) {
      cli::cli_abort("If {.arg quantile_levels} is `NULL`, {.arg values} must
                     be a data.frame with numeric names.")
    }
  }
  invisible(NULL)
}

#' @export
format.vctrs_quantiles <- function(x, ...) {
  quantile_levels <- attr(x, "levels")
  if (length(quantile_levels) == 1L) {
    x <- unlist(x)
    out <- round(x, 3L)
    out[is.na(x)] <- NA
  } else {
    rng <- sapply(x, range)
    out <- paste0("[", round(rng[1, ], 3L), ", ", round(rng[2, ], 3L), "]")
    out[is.na(rng[1, ]) | is.na(rng[2, ])] <- NA
  }
  out
}

#' @importFrom vctrs obj_print_footer
#' @export
vctrs::obj_print_footer

#' @export
obj_print_footer.vctrs_quantiles <- function(x, ...) {
  lvls <- attr(x, "quantile_levels")
  cat("# Quantile levels: ", format(lvls, digits = 3), "\n", sep = " ")
}

# Assumes the columns have the same order as quantile_level
restructure_rq_pred <- function(x, object) {
  quantile_level <- object$spec$quantile_level
  res <- tibble(.pred_quantile = vec_quantiles(x, quantile_level))
  res
}

