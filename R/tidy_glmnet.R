#' tidy methods for glmnet models
#'
#' `tidy()` methods for the various `glmnet` models that return the coefficients
#' for the specific penalty value used by the parsnip model fit.
#' @param x A fitted parsnip model that used the `glmnet` engine.
#' @param penalty A _single_ numeric value. If none is given, the value specified
#' in the model specification is used.
#' @param ... Not used
#' @return A tibble with columns `term`, `estimate`, and `penalty`. When a
#' multinomial mode is used, an additional `class` column is included.
#' @keywords internal
#' @export
tidy._elnet <- function(x, penalty = NULL, ...) {
  tidy_glmnet(x, penalty)
}

#' @export
#' @rdname tidy._elnet
tidy._lognet <- function(x, penalty = NULL, ...) {
  tidy_glmnet(x, penalty)
}

#' @export
#' @rdname tidy._elnet
tidy._multnet <- function(x, penalty = NULL, ...) {
  tidy_glmnet(x, penalty)
}

#' @export
#' @rdname tidy._elnet
tidy._fishnet <- function(x, penalty = NULL, ...) {
  tidy_glmnet(x, penalty)
}

#' @export
#' @rdname tidy._elnet
tidy._coxnet <- function(x, penalty = NULL, ...) {
  tidy_glmnet(x, penalty)
}

## -----------------------------------------------------------------------------

get_glmn_coefs <- function(x, penalty = 0.01) {
  res <- coef(x, s = penalty)
  res <- as.matrix(res)
  colnames(res) <- "estimate"
  rn <- rownames(res)
  res <- tibble::as_tibble(res) |> mutate(term = rn, penalty = penalty)
  res <- dplyr::select(res, term, estimate, penalty)
  if (is.list(res$estimate)) {
    res$estimate <- purrr::map(
      res$estimate,
      \(x) as_tibble(as.matrix(x), rownames = "term")
    )
    res <- tidyr::unnest(res, cols = c(estimate), names_repair = "minimal")
    names(res) <- c("class", "term", "estimate", "penalty")
  }
  res
}

tidy_glmnet <- function(x, penalty = NULL, ..., call = caller_env()) {
  check_installs(x$spec)
  load_libs(x$spec, quiet = TRUE, attach = TRUE)
  if (is.null(penalty)) {
    penalty <- x$spec$args$penalty
  }
  check_number_decimal(penalty, min = 0, allow_null = TRUE, call = call)
  get_glmn_coefs(x$fit, penalty = penalty)
}
