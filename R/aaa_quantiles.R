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

#' @importFrom vctrs vec_ptype_abbr
#' @export
vctrs::vec_ptype_abbr

#' @importFrom vctrs vec_ptype_full
#' @export
vctrs::vec_ptype_full


#' @export
vec_ptype_abbr.vctrs_quantiles <- function(x, ...) "qntls"

#' @export
vec_ptype_full.vctrs_quantiles <- function(x, ...) "quantiles"

new_vec_quantiles <- function(values = list(), quantile_levels = double()) {
  quantile_levels <- vctrs::vec_cast(quantile_levels, double())
  vctrs::new_vctr(
    values, quantile_levels = quantile_levels, class = "vctrs_quantiles"
  )
}


#' Create a vector containing sets of quantiles
#'
#' @param values A matrix of values. Each column should correspond to one of
#'   the quantile levels.
#' @param quantile_levels A vector of probabilities corresponding to `values`.
#'
#' @export
#' @return A vector of values associated with the quantile levels.
#'
#' @examples
#' v <- vec_quantiles(matrix(rnorm(20), 5), c(.2, .4, .6, .8))
#'
#' # Access the underlying information
#' attr(v, "quantile_levels")
#' vctrs::vec_data(v)
vec_quantiles <- function(values, quantile_levels = double()) {
  check_vec_quantiles_inputs(values, quantile_levels)
  quantile_levels <- vctrs::vec_cast(quantile_levels, double())
  num_lvls <- length(quantile_levels)

  if (ncol(values) != num_lvls) {
    cli::cli_abort(
      "The number of columns in {.arg values} must be equal to the length of
        {.arg quantile_levels}."
    )
  }
  values <- lapply(vctrs::vec_chop(values), drop)
  new_vec_quantiles(values, quantile_levels)
}

check_vec_quantiles_inputs <- function(values, levels, call = caller_env()) {
  if (!is.matrix(values)) {
    cli::cli_abort(
      "{.arg values} must be a {.cls matrix}, not {.obj_type_friendly {values}}.",
      call = call
    )
  }
  purrr::walk(levels,
    ~ check_number_decimal(.x, min = 0, max = 1, arg = "quantile_levels", call = call)
  )
  if (is.unsorted(levels)) {
    cli::cli_abort(
      "{.arg quantile_levels} must be sorted in increasing order.",
      call = call
    )
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

restructure_rq_pred <- function(x, object) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  rownames(x) <- NULL
  n_pred_quantiles <- ncol(x)
  # TODO check p = length(quantile_level)
  quantile_level <- object$spec$quantile_level
  tibble::tibble(.pred_quantile = vec_quantiles(x, quantile_level))
}

