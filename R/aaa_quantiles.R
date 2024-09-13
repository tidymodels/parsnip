# Helpers for quantile regression models

check_quantile_level <- function(x, object, call) {
  if (object$mode != "quantile regression") {
    return(invisible(TRUE))
  } else {
    if (is.null(x)) {
      cli::cli_abort("In {.fn check_mode}, at least one value of
      {.arg quantile_level} must be specified for quantile regression models.")
    }
  }
  if (any(is.na(x))) {
    cli::cli_abort("Missing values are not allowed in {.arg quantile_levels}.",
                   call = call)
  }
  x <- sort(unique(x))
  check_vector_probability(x, arg = "quantile_level", call = call)
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
vec_ptype_abbr.quantile_pred <- function(x, ...) {
  n_lvls <- length(attr(x, "quantile_levels"))
  cli::format_inline("qtl{?s}({n_lvls})")
}

#' @export
vec_ptype_full.quantile_pred <- function(x, ...) "quantiles"

new_quantile_pred <- function(values = list(), quantile_levels = double()) {
  quantile_levels <- vctrs::vec_cast(quantile_levels, double())
  vctrs::new_vctr(
    values, quantile_levels = quantile_levels, class = "quantile_pred"
  )
}

#' Create a vector containing sets of quantiles
#'
#' [quantile_pred()] is a special vector class used to efficiently store
#' predictions from a quantile regression model. It requires the same quantile
#' levels for each row being predicted.
#'
#' @param values A matrix of values. Each column should correspond to one of
#'   the quantile levels.
#' @param quantile_levels A vector of probabilities corresponding to `values`.
#' @param x An object produced by [quantile_pred()].
#' @param .rows,.name_repair,rownames Arguments not used but required by the
#' original S3 method.
#' @param ... Not currently used.
#'
#' @export
#' @return
#'   * [quantile_pred()] returns a vector of values associated with the
#' quantile levels.
#'   * [extract_quantile_levels()] returns a numeric vector of levels.
#'   * [as_tibble()] returns a tibble with rows `".pred_quantile"`,
#'   `".quantile_levels"`, and `".row"`.
#'   * [as.matrix()] returns an unnamed matrix with rows as sames, columns as
#'   quantile levels, and entries are predictions.
#' @examples
#' .pred_quantile <- quantile_pred(matrix(rnorm(20), 5), c(.2, .4, .6, .8))
#'
#' unclass(.pred_quantile)
#'
#' # Access the underlying information
#' extract_quantile_levels(.pred_quantile)
#'
#' # Matrix format
#' as.matrix(.pred_quantile)
#'
#' # Tidy format
#' tibble::as_tibble(.pred_quantile)
quantile_pred <- function(values, quantile_levels = double()) {
  check_quantile_pred_inputs(values, quantile_levels)

  quantile_levels <- vctrs::vec_cast(quantile_levels, double())
  num_lvls <- length(quantile_levels)

  if (ncol(values) != num_lvls) {
    cli::cli_abort(
      "The number of columns in {.arg values} must be equal to the length of
        {.arg quantile_levels}."
    )
  }
  rownames(values) <- NULL
  colnames(values) <- NULL
  values <- lapply(vctrs::vec_chop(values), drop)
  new_quantile_pred(values, quantile_levels)
}

check_quantile_pred_inputs <- function(values, levels, call = caller_env()) {
  if (any(is.na(levels))) {
    cli::cli_abort("Missing values are not allowed in {.arg quantile_levels}.",
                   call = call)
  }

  if (!is.matrix(values)) {
    cli::cli_abort(
      "{.arg values} must be a {.cls matrix}, not {.obj_type_friendly {values}}.",
      call = call
    )
  }
  check_vector_probability(levels, arg = "quantile_levels", call = call)

  if (is.unsorted(levels)) {
    cli::cli_abort(
      "{.arg quantile_levels} must be sorted in increasing order.",
      call = call
    )
  }
  invisible(NULL)
}

#' @export
format.quantile_pred <- function(x, ...) {
  quantile_levels <- attr(x, "quantile_levels")
  if (length(quantile_levels) == 1L) {
    x <- unlist(x)
    out <- round(x, 3L)
    out[is.na(x)] <- NA_real_
  } else {
    rng <- sapply(x, range, na.rm = TRUE)
    out <- paste0("[", round(rng[1, ], 3L), ", ", round(rng[2, ], 3L), "]")
    out[is.na(rng[1, ]) & is.na(rng[2, ])] <- NA_character_
    m <- median(x)
    out <- paste0("[", round(m, 3L), "]")
  }
  out
}

#' @importFrom vctrs obj_print_footer
#' @export
vctrs::obj_print_footer

#' @export
obj_print_footer.quantile_pred <- function(x, digits = 3, ...) {
  lvls <- attr(x, "quantile_levels")
  cat("# Quantile levels: ", format(lvls, digits = digits), "\n", sep = " ")
}

check_vector_probability <- function(x, ...,
                                     allow_na = FALSE,
                                     allow_null = FALSE,
                                     arg = caller_arg(x),
                                     call = caller_env()) {
  for (d in x) {
    check_number_decimal(
      d, min = 0, max = 1,
      arg = arg, call = call,
      allow_na = allow_na,
      allow_null = allow_null,
      allow_infinite = FALSE
    )
  }
}

#' @export
median.quantile_pred <- function(x, ...) {
  lvls <- attr(x, "quantile_levels")
  loc_median <- (abs(lvls - 0.5) < sqrt(.Machine$double.eps))
  if (any(loc_median)) {
    return(map_dbl(x, ~ .x[min(which(loc_median))]))
  }
  if (length(lvls) < 2 || min(lvls) > 0.5 || max(lvls) < 0.5) {
    return(rep(NA, vctrs::vec_size(x)))
  }
  map_dbl(x, ~ stats::approx(lvls, .x, xout = 0.5)$y)
}

restructure_rq_pred <- function(x, object) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  rownames(x) <- NULL
  n_pred_quantiles <- ncol(x)
  quantile_level <- object$spec$quantile_level

  tibble::new_tibble(x = list(.pred_quantile = quantile_pred(x, quantile_level)))
}

#' @export
#' @rdname quantile_pred
extract_quantile_levels <- function(x) {
  if (!inherits(x, "quantile_pred")) {
    cli::cli_abort("{.arg x} should have class {.val quantile_pred}.")
  }
  attr(x, "quantile_levels")
}

#' @export
#' @rdname quantile_pred
as_tibble.quantile_pred <-
  function (x, ..., .rows = NULL, .name_repair = "minimal", rownames = NULL) {
    lvls <- attr(x, "quantile_levels")
    n_samp <- length(x)
    n_quant <- length(lvls)
    tibble::tibble(
      .pred_quantile = unlist(x),
      .quantile_levels = rep(lvls, n_samp),
      .row = rep(1:n_samp, each = n_quant)
    )
  }

#' @export
#' @rdname quantile_pred
as.matrix.quantile_pred <- function(x, ...) {
  num_samp <- length(x)
  matrix(unlist(x), nrow = num_samp)
}
