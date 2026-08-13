#' Ordinal regression via ordinalNet
#'
#' [ordinalNet::ordinalNet()] uses an original coordinate descent algorithm to
#' fit models of the elementwise link multinomial-ordinal (ELMO) class, each
#' comprising parallel and non-parallel forms and including the most common
#' ordinal regression models, with an elastic net penalty.
#'
#' @includeRmd man/rmd/ordinal_reg_ordinalNet.md details
#'
#' @name details_ordinal_reg_ordinalNet
#' @keywords internal
NULL

# See inst/README-DOCS.md for a description of how these files are processed.

# ------------------------------------------------------------------------------
# Helpers for the `ordinalNet` engine
#
# These functions match standardized dials parameter values to values native
# to `ordinalNet::ordinalNet()`. They are used by `translate.ordinal_reg()`.

match_ordinal_link_ordinalNet <- function(link) {
  if (!is.character(link)) {
    return(link)
  }
  link <- match.arg(link, dials::values_ordinal_link)
  if (link == "logistic") {
    link <- "logit"
  }
  if (link == "loglog") {
    cli::cli_abort(
      c(
        "The `ordinalNet` engine does not support the log-log ordinal link.",
        "i" = "See `?ordinalNet::ordinalNet` for provided link functions."
      )
    )
  }
  link
}
