matrix_to_quantile_pred <- function(x, object) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  rownames(x) <- NULL
  n_pred_quantiles <- ncol(x)
  quantile_levels <- object$spec$quantile_levels

  tibble::new_tibble(x = list(.pred_quantile = hardhat::quantile_pred(x, quantile_levels)))
}
