#' Determine required packages for a model
#'
#' @param x A [model specification][model_spec] or [fit][model_fit].
#' @param infra Should parsnip itself be included in the result?
#' @param ... Not used.
#' @return A character vector
#' @name required_pkgs.model_spec
#' @examplesIf !parsnip:::is_cran_check()
#' should_fail <- try(required_pkgs(linear_reg(engine = NULL)), silent = TRUE)
#' should_fail
#'
#' linear_reg() |>
#'   set_engine("glmnet") |>
#'   required_pkgs()
#'
#' linear_reg() |>
#'   set_engine("glmnet") |>
#'   required_pkgs(infra = FALSE)
#'
#' linear_reg() |>
#'   set_engine("lm") |>
#'   fit(mpg ~ ., data = mtcars) |>
#'   required_pkgs()
#' @export
required_pkgs.model_spec <- function(x, infra = TRUE, ...) {
  if (is.null(x$engine)) {
    cli::cli_abort("Please set an engine.")
  }
  check_bool(infra)
  get_pkgs(x, infra)
}

#' @export
#' @rdname required_pkgs.model_spec
required_pkgs.model_fit <- function(x, infra = TRUE, ...) {
  check_bool(infra)
  get_pkgs(x$spec, infra)
}

get_pkgs <- function(x, infra) {
  cls <- class(x)[1]
  pkgs <- get_from_env(paste0(cls, "_pkgs"))
  pkgs <- vctrs::vec_slice(pkgs, pkgs$engine == x$engine)

  if (length(pkgs$pkg) == 0) {
    res <- character(0)
  } else {
    res <- pkgs$pkg[[1]]
  }
  if (length(res) == 0) {
    res <- character(0)
  }
  if (infra) {
    infra_pkgs <- c("parsnip")
    res <- c(infra_pkgs, res)
  }
  res <- unique(res)
  res <- res[length(res) != 0]
  res
}
