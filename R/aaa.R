
maybe_multivariate <- function(results, object) {
  if (isTRUE(ncol(results) > 1))
    results <- as_tibble(results)
  else
    results <- unname(results[, 1])
  results
}

#' @importFrom stats quantile
convert_stan_interval <- function(x, level = 0.95, lower = TRUE) {
  alpha <- (1 - level) / 2
  if (!lower) {
    alpha <- 1 - alpha
  }
  res <- apply(x, 2, quantile, probs = alpha, na.rm = TRUE)
  res <- unname(res)
  res
}

