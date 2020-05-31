
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

#' Make a table of arguments
#' @param model_name A character string for the model
#' @keywords internal
#' @export
convert_args <- function(model_name) {
  envir <- get_model_env()

  args <-
    ls(envir) %>%
    tibble::tibble(name = .) %>%
    dplyr::filter(grepl("args", name)) %>%
    dplyr::mutate(model = sub("_args", "", name),
                  args  = purrr::map(name, ~envir[[.x]])) %>%
    tidyr::unnest(args) %>%
    dplyr::select(model:original)

  convert_df <- args %>%
    dplyr::filter(grepl(model_name, model)) %>%
    dplyr::select(-model) %>%
    tidyr::pivot_wider(names_from = engine, values_from = original)

  convert_df %>%
    knitr::kable(col.names = paste0("**", colnames(convert_df), "**"))

}


# ------------------------------------------------------------------------------
# nocov

#' @importFrom utils globalVariables
utils::globalVariables(
  c('.', '.label', '.pred', '.row', 'data', 'engine', 'engine2', 'group',
    'lab', 'original', 'predicted_label', 'prediction', 'value', 'type',
    "neighbors", ".submodels", "has_submodel", "max_neighbor", "max_penalty",
    "max_terms", "max_tree", "model", "name", "num_terms", "penalty", "trees",
    "sub_neighbors", ".pred_class", "x", "y", "predictor_indicators")
)

# nocov end
