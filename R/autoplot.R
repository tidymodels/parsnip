#' Create a ggplot for a model object
#'
#' This method provides a good visualization method for model results.
#' Currently, only methods for glmnet models are implemented.
#'
#' @param object A model fit object.
#' @param min_penalty A single, non-negative number for the smallest penalty
#' value that should be shown in the plot. If left `NULL`, the whole data
#' range is used.
#' @param best_penalty A single, non-negative number that will show a vertical
#' line marker. If left `NULL`, no line is shown. When this argument is used,
#' the \pkg{ggrepl} package is required.
#' @param top_n A non-negative integer for how many model predictors to label.
#' The top predictors are ranked by their absolute coefficient value. For
#' multinomial or multivariate models, the `top_n` terms are selected within
#' class or response, respectively.
#' @param ... For [autoplot.glmnet()], options to pass to
#' [ggrepel::geom_label_repel()]. Otherwise, this argument is ignored.
#' @return A ggplot object with penalty on the x-axis and coefficients on the
#' y-axis. For multinomial or multivariate models, the plot is faceted.
#' @details The \pkg{glmnet} package will need to be attached or loaded for
#' its `autoplot()` method to work correctly.
#'
#' @export
autoplot.model_fit <- function(object, ...) {
  autoplot(object$fit, ...)
}

# glmnet is not a formal dependency here.
# unit tests are located at https://github.com/tidymodels/extratests
# nocov start

#' @export
#' @rdname autoplot.model_fit
autoplot.glmnet <- function(object, ..., min_penalty = 0, best_penalty = NULL,
                            top_n = 3L) {
  check_number_decimal(min_penalty, min = 0, max = 1)
  check_number_decimal(best_penalty, min = 0, max = 1, allow_null = TRUE)
  check_number_whole(top_n, min = 1, max = Inf, allow_infinite = TRUE)
  autoplot_glmnet(object, min_penalty, best_penalty, top_n, ...)
}


map_glmnet_coefs <- function(x, call = rlang::caller_env()) {
  coefs <- coef(x)
  # If parsnip is used to fit the model, glmnet should be attached and this will
  # work. If an object is loaded from a new session, they will need to load the
  # package.
  if (is.null(coefs)) {
    cli::cli_abort(
      "Please load the {.pkg glmnet} package before running {.fun autoplot}.",
      call = call
    )
  }
  p <- x$dim[1]
  if (is.list(coefs)) {
    classes <- names(coefs)
    coefs <- purrr::map(coefs, reformat_coefs, p = p, penalty = x$lambda)
    coefs <- purrr::map2_dfr(coefs, classes, ~ dplyr::mutate(.x, class = .y))
  } else {
    coefs <- reformat_coefs(coefs, p = p, penalty = x$lambda)
  }
  coefs
}

reformat_coefs <- function(x, p, penalty) {
  x <- as.matrix(x)
  num_estimates <- nrow(x)
  if (num_estimates > p) {
    # The intercept is first
    x <- x[-(num_estimates - p),, drop = FALSE]
  }
  term_lab <- rownames(x)
  colnames(x) <- paste(seq_along(penalty))
  x <- tibble::as_tibble(x)
  x$term <- term_lab
  x <- tidyr::pivot_longer(x, cols = -term, names_to = "index", values_to = "estimate")
  x$penalty <- rep(penalty, p)
  x$index <- NULL
  x
}

top_coefs <- function(x, top_n = 5) {
  x |>
    dplyr::group_by(term) |>
    dplyr::arrange(term, dplyr::desc(abs(estimate))) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(abs(estimate))) |>
    dplyr::slice(seq_len(top_n))
}

autoplot_glmnet <- function(x, min_penalty = 0, best_penalty = NULL, top_n = 3L,
                            call = rlang::caller_env(), ...) {
  tidy_coefs <-
    map_glmnet_coefs(x, call = call) |>
    dplyr::filter(penalty >= min_penalty)

  actual_min_penalty <- min(tidy_coefs$penalty)
  num_terms <- length(unique(tidy_coefs$term))
  top_n <- min(top_n[1], num_terms)
  if (top_n < 0) {
    top_n <- 0
  }

  has_groups <- any(names(tidy_coefs) == "class")

  # Keep the large values
  if (has_groups) {
    label_coefs <-
      tidy_coefs |>
      dplyr::group_nest(class) |>
      dplyr::mutate(data = purrr::map(data, top_coefs, top_n = top_n)) |>
      dplyr::select(class, data) |>
      tidyr::unnest(cols = data)
  } else {
    if (is.null(best_penalty)) {
      label_coefs <- tidy_coefs |>
        top_coefs(top_n)
    } else {
      label_coefs <- tidy_coefs |>
        dplyr::filter(penalty > best_penalty) |>
        dplyr::filter(penalty == min(penalty)) |>
        dplyr::arrange(dplyr::desc(abs(estimate))) |>
        dplyr::slice(seq_len(top_n))
    }
  }

  label_coefs <-
    label_coefs |>
    dplyr::mutate(penalty = best_penalty %||% actual_min_penalty) |>
    dplyr::mutate(label = gsub(".pred_no_", "", term))

  # plot the paths and highlight the large values
  p <-
    tidy_coefs |>
    ggplot2::ggplot(ggplot2::aes(x = penalty, y = estimate, group = term, col = term))

  if (has_groups) {
    p <- p + ggplot2::facet_wrap(~ class)
  }

  if (!is.null(best_penalty)) {
    p <- p + ggplot2::geom_vline(xintercept = best_penalty, lty = 3)
  }

  p <- p +
    ggplot2::geom_line(alpha = .4, show.legend = FALSE) +
    ggplot2::scale_x_log10()

  if(top_n > 0) {
    rlang::check_installed("ggrepel")
    p <- p +
      ggrepel::geom_label_repel(
        data = label_coefs,
        ggplot2::aes(y = estimate, label = label),
        show.legend = FALSE,
        ...
      )
  }
  p
}

# nocov end
