
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

#' @rdname convert_args
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
    dplyr::filter(grepl(model_name, model)) %>%
    tidyr::unnest(args) %>%
    dplyr::select(model:original) %>%
    full_join(get_arg_defaults(model_name)) %>%
    mutate(original = dplyr::if_else(!is.na(default),
                                     paste0(original, " (", default, ")"),
                                     original)) %>%
    select(-default)

  convert_df <- args %>%
    dplyr::select(-model) %>%
    tidyr::pivot_wider(names_from = engine, values_from = original)

  convert_df %>%
    knitr::kable(col.names = paste0("**", colnames(convert_df), "**"))

}

#' @rdname convert_args
#' @keywords internal
#' @export
get_arg_defaults <- function(model) {
  check_model_exists(model)
  gdf <- get(paste0("get_defaults_", model))
  gdf()
}

#' @rdname convert_args
#' @keywords internal
#' @export
get_arg <- function(ns, f, arg) {
  args <- formals(getFromNamespace(f, ns))
  args <- as.list(args)
  as.character(args[[arg]])
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
