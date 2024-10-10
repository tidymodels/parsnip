#' parsnip
#'
#' The goal of parsnip is to provide a tidy, unified interface to models that
#' can be used to try a range of models without getting bogged down in the
#' syntactical minutiae of the underlying packages.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom dplyr arrange bind_cols bind_rows collect full_join group_by
#' @importFrom dplyr mutate pull rename select starts_with summarise tally
#' @importFrom generics tunable varying_args tune_args
#' @importFrom ggplot2 autoplot
#' @importFrom glue glue_collapse
#' @importFrom lifecycle deprecated
#' @importFrom pillar type_sum
#' @importFrom purrr as_vector imap imap_lgl map map_chr map_dbl
#' @importFrom purrr map_lgl %||%
#' @importFrom stats .checkMFClasses .getXlevels as.formula binomial coef
#' @importFrom stats delete.response model.frame model.matrix model.offset
#' @importFrom stats model.response model.weights na.omit na.pass predict qnorm
#' @importFrom stats qt quantile setNames terms update
#' @importFrom tibble as_tibble is_tibble tibble
#' @importFrom tidyr gather
#' @importFrom utils capture.output getFromNamespace globalVariables head
#' @importFrom utils methods stack
#' @importFrom vctrs vec_size vec_unique
## usethis namespace: end
NULL

# nocov start

utils::globalVariables(
  c(
    '.', '.label', '.pred', '.row', 'data', 'engine', 'engine2', 'group',
    'lab', 'original', 'predicted_label', 'prediction', 'value', 'type',
    "neighbors", ".submodels", "has_submodel", "max_neighbor", "max_penalty",
    "max_terms", "max_tree", "model", "name", "num_terms", "penalty", "trees",
    "sub_neighbors", ".pred_class", "x", "y", "predictor_indicators",
    "compute_intercept", "remove_intercept", "estimate", "term",
    "call_info", "component", "component_id", "func", "tunable", "label",
    "pkg", ".order", "item", "tunable", "has_ext", "id", "weights", "has_wts",
    "protect", "weight_time", ".prob_cens", ".weight_cens", "s"
  )
)

release_bullets <- function() {
  c(
    "Update parsnip add-in information at `inst/add-in/parsnip_model_db.R`",
    "Run `update_model_info_file()` and verify results",
    "Run `knit_engine_docs()` and `devtools::document()` to update docs"
  )
}

# nocov end

