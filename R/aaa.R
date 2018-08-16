
maybe_multivariate <- function(results, object) {
  if (isTRUE(ncol(results) > 1))
    results <- as_tibble(results)
  else
    results <- unname(results[, 1])
  results
}
