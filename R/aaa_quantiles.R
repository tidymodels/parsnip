#' Reformat quantile predictions
#'
#' @param x A matrix of predictions with rows as samples and columns as quantile
#' levels.
#' @param object A parsnip `model_fit` object from a quantile regression model.
#' @keywords internal
#' @export
matrix_to_quantile_pred <- function(x, object) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  rownames(x) <- NULL
  n_pred_quantiles <- ncol(x)
  quantile_levels <- object$spec$quantile_levels

  tibble::new_tibble(x = list(.pred_quantile = hardhat::quantile_pred(x, quantile_levels)))
}
