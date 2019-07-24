
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

# ------------------------------------------------------------------------------
# min_grid generic - put here so that the generic shows up first in the man file

#' Determine the minimum set of model fits
#'
#' `min_grid` determines exactly what models should be fit in order to
#'  evaluate the entire set of tuning parameter combinations. This is for
#'  internal use only and the API may change in the near future.
#' @param x A model specification.
#' @param grid A tibble with tuning parameter combinations.
#' @param ... Not currently used.
#' @return A tibble with the minimum tuning parameters to fit and an additional
#' list column with the parameter combinations used for prediction.
#' @keywords internal
#' @export
min_grid <- function(x, grid, ...) {
  # x is a `model_spec` object from parsnip
  # grid is a tibble of tuning parameter values with names
  #  matching the parameter names.
  UseMethod("min_grid")
}

# As an example, if we fit a boosted tree  model and tune over
# trees = 1:20 and min_n = c(20, 30)
# we should only have to fit two models:
#
#   trees = 20 & min_n = 20
#   trees = 20 & min_n = 30
#
# The logic related to how this "mini grid" gets made is model-specific.
#
# To get the full set of predictions, we need to know, for each of these two
# models, what values of num_terms to give to the multi_predict() function.
#
# The current idea is to have a list column of the extra models for prediction.
# For the example above:
#
#   # A tibble: 2 x 3
#     trees min_n .submodels
#     <dbl> <dbl> <list>
#   1    20    20 <named list [1]>
#   2    20    30 <named list [1]>
#
# and the .submodels would both be
#
#  list(trees = 1:19)
#
# There are a lot of other things to consider in future versions like grids
# where there are multiple columns with the same name (maybe the results of
# a recipe) and so on.

# ------------------------------------------------------------------------------
# helper functions

# Template for model results that do no have the sub-model feature
blank_submodels <- function(grid) {
  grid %>%
    dplyr::mutate(.submodels = map(1:nrow(grid), ~ list()))
}

get_fixed_args <- function(info) {
  # Get non-sub-model columns to iterate over
  fixed_args <- info$name[!info$has_submodel]
}

get_submodel_info <- function(spec, grid) {
  param_info <-
    get_from_env(paste0(class(spec)[1], "_args")) %>%
    dplyr::filter(engine == spec$engine) %>%
    dplyr::select(name = parsnip, has_submodel)

  # In case a recipe or other activity has grid parameter columns,
  # add those to the results
  grid_names <- names(grid)
  is_mod_param <- grid_names %in% param_info$name
  if (any(!is_mod_param)) {
    param_info <-
      param_info %>%
      dplyr::bind_rows(
        tibble::tibble(name = grid_names[!is_mod_param],
                       has_submodel = FALSE)
      )
  }
  param_info %>% dplyr::filter(name %in% grid_names)
}


# ------------------------------------------------------------------------------
# nocov

#' @importFrom utils globalVariables
utils::globalVariables(
  c('.', '.label', '.pred', '.row', 'data', 'engine', 'engine2', 'group',
    'lab', 'original', 'predicted_label', 'prediction', 'value', 'type',
    "neighbors", ".submodels", "has_submodel", "max_neighbor", "max_penalty",
    "max_terms", "max_tree", "name", "num_terms", "penalty", "trees",
    "sub_neighbors")
  )

# nocov end
