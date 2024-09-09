# nocov start
# tested in tidymodels/extratests#67

new_reverse_km_fit <-
  function(formula,
           object,
           pkgs = character(0),
           label = character(0),
           extra_cls = character(0)) {
    res <- list(formula = formula, fit = object, label = label, required_pkgs = pkgs)
    class(res) <- c(paste0("censoring_model_", label), "censoring_model", extra_cls)
    res
  }

# ------------------------------------------------------------------------------
# estimate the reverse km curve for censored regression models

reverse_km <- function(obj, eval_env) {
  if (obj$mode != "censored regression") {
    return(list())
  }
  rlang::check_installed("prodlim")

  # Note: even when fit_xy() is called, eval_env will still have
  # objects data and formula in them
  f <- eval_env$formula
  km_form <- stats::update(f, ~ 1)
  cl <-
    rlang::call2(
      "prodlim",
      formula = km_form,
      .ns = "prodlim",
      reverse = TRUE,
      type = "surv",
      x = FALSE,
      data = rlang::expr(eval_env$data)
    )

  if (!is.null(eval_env$weights)) {
    cl <- rlang::call_modify(cl, caseweights = rlang::expr(eval_env$weights))
  }
  rkm <- try(rlang::eval_tidy(cl), silent = TRUE)
  new_reverse_km_fit(f, object = rkm, label = "reverse_km", pkgs = "prodlim")
}

# ------------------------------------------------------------------------------
# Basic S3 methods

#' @export
print.censoring_model <- function(x, ...) {
  cat(x$label, "model for predicting the probability of censoring\n")
  invisible(x)
}

#' @export
predict.censoring_model <- function(object, ...) {
  cli::cli_abort(
    "Don't know how to predict with a censoring model of type {object$label}."
  )
  invisible(NULL)
}

#' @export
predict.censoring_model_reverse_km <- function(object, new_data, time, as_vector = FALSE, ...) {
  rlang::check_dots_empty()

  rlang::check_installed("prodlim", version = "2022.10.13")
  rlang::check_installed("censored", version = "0.1.1.9002")

  if (lifecycle::is_present(new_data)) {
    lifecycle::deprecate_stop(
      "1.2.0",
      "predict.censoring_model_reverse_km(new_data)"
    )
  }

  res <- rep(NA_real_, length(time))
  if (length(time) == 0) {
    return(res)
  }

  # Some time values might be NA (for Graf category 2)
  is_na <- which(is.na(time))
  if (length(is_na) > 0) {
    time <- time[-is_na]
  }

  tmp <- purrr::map_dbl(time, ~ predict(object$fit, times = .x, type = "surv"))

  zero_prob <- purrr::map_lgl(tmp, ~ !is.na(.x) && .x == 0)
  if (any(zero_prob)) {
    # Don't want censoring probabilities of zero so add an epsilon
    # Either use 1/n or half of the minimum survival probability
    n <- max(object$fit$n.risk)
    half_min_surv_prob <- min(object$fit$surv[object$fit$surv > 0]) / 2
    eps <- min(1 / n, half_min_surv_prob)
    tmp[zero_prob] <- eps
  }

  if (length(is_na) > 0) {
    res[-is_na] <- tmp
  } else {
    res <- tmp
  }

  if (!as_vector) {
    res <- tibble::tibble(.prob_censored = unname(res))
  }
  res
}

# nocov end
