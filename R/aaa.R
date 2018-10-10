
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

descr_env <- new_environment()

reset_descr_env <- function() {
  env_poke(descr_env, ".data",  NULL)
  env_poke(descr_env, ".x",     NULL)
  env_poke(descr_env, ".y",     NULL)
  env_poke(descr_env, ".cols",  NULL)
  env_poke(descr_env, ".preds", NULL)
  env_poke(descr_env, ".obs",   NULL)
  env_poke(descr_env, ".levs",  NULL)
  env_poke(descr_env, ".facts", NULL)
  invisible(NULL)
}

reset_descr_env()
