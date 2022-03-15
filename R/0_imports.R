#' @importFrom dplyr arrange bind_cols bind_rows collect full_join group_by
#' @importFrom dplyr mutate pull rename select starts_with summarise tally
#' @importFrom generics varying_args
#' @importFrom glue glue_collapse
#' @importFrom purrr as_vector imap imap_lgl map map_chr map_dbl map_df map_dfr
#' @importFrom purrr map_lgl %||%
#' @importFrom rlang abort call2 caller_env current_env enquo enquos eval_tidy
#' @importFrom rlang expr get_expr is_empty is_missing is_null is_quosure
#' @importFrom rlang is_symbolic lgl missing_arg quo_get_expr quos sym syms
#' @importFrom stats .checkMFClasses .getXlevels as.formula binomial coef
#' @importFrom stats delete.response model.frame model.matrix model.offset
#' @importFrom stats model.response model.weights na.omit na.pass predict qnorm
#' @importFrom stats qt quantile setNames terms update
#' @importFrom tibble as_tibble is_tibble tibble type_sum
#' @importFrom tidyr gather
#' @importFrom utils capture.output getFromNamespace globalVariables head
#' @importFrom utils methods stack
#' @importFrom vctrs vec_size vec_unique
#' @importFrom ggplot2 autoplot
NULL

# ------------------------------------------------------------------------------

release_bullets <- function() {
  c(
    "Update parsnip add-in information at `inst/add-in/parsnip_model_db.R`",
    "Run `update_model_info_file()` and verify results",
    "Run `knit_engine_docs()` and `devtools::document()` to update docs"
  )
}
