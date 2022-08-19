
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

# used by logistic_reg() and gen_additive_mod()
logistic_lp_to_conf_int <- function(results, object) {
  hf_lvl <- (1 - object$spec$method$pred$conf_int$extras$level)/2
  const <-
    stats::qt(hf_lvl, df = object$fit$df.residual, lower.tail = FALSE)
  trans <- object$fit$family$linkinv
  res_2 <-
    tibble(
      lo = trans(results$fit - const * results$se.fit),
      hi = trans(results$fit + const * results$se.fit)
    )
  res_1 <- res_2
  res_1$lo <- 1 - res_2$hi
  res_1$hi <- 1 - res_2$lo
  lo_nms <- paste0(".pred_lower_", object$lvl)
  hi_nms <- paste0(".pred_upper_", object$lvl)
  colnames(res_1) <- c(lo_nms[1], hi_nms[1])
  colnames(res_2) <- c(lo_nms[2], hi_nms[2])
  res <- bind_cols(res_1, res_2)

  if (object$spec$method$pred$conf_int$extras$std_error)
    res$.std_error <- results$se.fit
  res
}

# used by linear_reg() and gen_additive_mod()
linear_lp_to_conf_int <-
function(results, object) {
  hf_lvl <- (1 - object$spec$method$pred$conf_int$extras$level)/2
  const <-
    stats::qt(hf_lvl, df = object$fit$df.residual, lower.tail = FALSE)
  trans <- object$fit$family$linkinv
  res <-
    tibble(
      .pred_lower = trans(results$fit - const * results$se.fit),
      .pred_upper = trans(results$fit + const * results$se.fit)
    )
  # In case of inverse or other links
  if (any(res$.pred_upper < res$.pred_lower)) {
    nms <- names(res)
    res <- res[, 2:1]
    names(res) <- nms
  }

  if (object$spec$method$pred$conf_int$extras$std_error) {
    res$.std_error <- results$se.fit
  }
  res
}

combine_words <- function(x) {
  if (isTRUE(length(x) > 2)) {
    last <- ", and "
  } else {
    last <- " and "
  }

  glue::glue_collapse(x, ", ", last = last)
}
