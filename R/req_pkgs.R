#' Determine required packages for a model
#'
#' @param x A model specification or fit.
#' @param ... Not used.
#' @return A character string of package names (if any).
#' @details
#' For a model specification, the engine must be set. The list produced by
#' `req_pkgs()`does not include the `parsnip` package while `required_pkgs()`
#' does.
#' @examples
#' should_fail <- try(req_pkgs(linear_reg()), silent = TRUE)
#' should_fail
#'
#' linear_reg() %>%
#'   set_engine("glmnet") %>%
#'   req_pkgs()
#'
#' linear_reg() %>%
#'   set_engine("lm") %>%
#'   fit(mpg ~ ., data = mtcars) %>%
#'   req_pkgs()
#' @export
req_pkgs <- function(x, ...) {
  UseMethod("req_pkgs")
}

#' @export
#' @rdname req_pkgs
req_pkgs.model_spec <- function(x, ...) {
  if (is.null(x$engine)) {
    rlang::abort("Please set an engine.")
  }
  get_pkgs(x)
}

#' @export
#' @rdname req_pkgs
req_pkgs.model_fit <- function(x, ...) {
  get_pkgs(x$spec)
}

get_pkgs <- function(x) {
  cls <- class(x)[1]
  pkgs <-
    get_from_env(paste0(cls, "_pkgs")) %>%
    dplyr::filter(engine == x$engine)
  res <- pkgs$pkg[[1]]
  if (length(res) == 0) {
    res <- character(0)
  }
  res
}

#' @export
#' @rdname req_pkgs
required_pkgs.model_spec <- function(x, ...) {
  res <- req_pkgs.model_spec(x, ...)
  sort(unique(c("parsnip", res)))
}

#' @export
#' @rdname req_pkgs
required_pkgs.model_fit <- function(x, ...) {
  res <- req_pkgs.model_fit(x, ...)
  sort(unique(c("parsnip", res)))
}
