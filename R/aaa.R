
maybe_multivariate <- function(results, object) {

  if (isTRUE(ncol(results) > 1)) {
    nms <- colnames(results)
    results <- as_tibble(results, .name_repair = "minimal")
    if (length(nms) == 0 && length(object$preproc$y_var) == ncol(results)) {
      names(results) <- object$preproc$y_var
    }
  }  else {
    results <- unname(results[, 1])
  }
  results
}

#' Convenience function for intervals
#' @importFrom stats quantile
#' @export
#' @keywords internal
#' @param x A fitted model object
#' @param level Level of uncertainty for intervals
#' @param lower Is `level` the lower level?
convert_stan_interval <- function(x, level = 0.95, lower = TRUE) {
  alpha <- (1 - level) / 2
  if (!lower) {
    alpha <- 1 - alpha
  }
  res <- apply(x, 2, quantile, probs = alpha, na.rm = TRUE)
  res <- unname(res)
  res
}

# ------------------------------------------------------------------------------
# nocov

#' @importFrom utils globalVariables
utils::globalVariables(
  c('.', '.label', '.pred', '.row', 'data', 'engine', 'engine2', 'group',
    'lab', 'original', 'predicted_label', 'prediction', 'value', 'type',
    "neighbors", ".submodels", "has_submodel", "max_neighbor", "max_penalty",
    "max_terms", "max_tree", "model", "name", "num_terms", "penalty", "trees",
    "sub_neighbors", ".pred_class", "x", "y")
)

# nocov end
